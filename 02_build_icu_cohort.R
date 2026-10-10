# =====================================================================
# 02_build_icu_cohort.R — 익일 ICU 전원 예측용 코호트 구축
#
# 예측 과제: index CXR 촬영일 다음 날(D+1) ward/ER → ICU 전원 여부 (이진 분류)
#   outcome = 1 : CXR 촬영일 + 1일에 non-ICU → ICU 전환 발생 (환자당 최초 1건)
#   outcome = 0 : 전 기간 ward/ER → ICU 전환 이력 없는 환자의 최초 eligible CXR
#                 (처음부터 ICU로 입실한 환자도 전원 사건이 없으므로 음성)
#   * 날짜(calendar day) 단위 정의. 전원 당일 및 ICU 재실 중 촬영 CXR 은 제외
#
# 구성:
#   [사전 확인] care_site 분포, ER visit_detail, 동일일 CXR, FK/NULL, ICU 판정 교차검증
#   [step1] SQL: ICU 판정 → 전원 사건 → eligible CXR → 양성/음성 index CXR (환자당 1행)
#   [step2] SQL: index CXR 기준 [D-7, D0] 최신 lab 18종 + 인구학 정보 + 영상 경로 결합
#   [저장]  icu_cohort.csv (person_id, image_occurrence_id, local_path, lab 등)
#
# concept 정의 방식: ATLAS(OHDSI) 에서 개념 검색/확정
#   1) ATLAS Search에서 검색
#   2) SNOMED standard concept 및 concept_id 선택
# =====================================================================


# --- 패키지 (없으면 설치) -----------------------------------------
# 순수한 R 환경을 가정한 후 미설치된 패키지 설치.

ensure <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}
for (p in c("DatabaseConnector", "dplyr")) ensure(p)


# --- 이 스크립트가 있는 폴더 찾기 ---------------------------------
# CSV 를 코드 파일과 같은 폴더에 만들기 위함.

get_script_dir <- function() {
  for (i in seq_len(sys.nframe())) {  # 1) source() 실행
    of <- sys.frame(i)$ofile
    if (!is.null(of)) return(dirname(normalizePath(of)))
  }
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)  # 2) Rscript --file=
  if (length(fa)) return(dirname(normalizePath(sub("^--file=", "", fa[1]))))
  if (requireNamespace("rstudioapi", quietly = TRUE) &&  # 3) RStudio 줄단위 실행
      rstudioapi::isAvailable()) {
    p <- rstudioapi::getSourceEditorContext()$path
    if (nzchar(p)) return(dirname(normalizePath(p)))
  }
  getwd()  # 4) 폴백
}
SCRIPT_DIR <- get_script_dir()

# 01_connect_icu_cdm.R 먼저 실행하여 conn / CDM_SCHEMA 준비 (같은 폴더 기준)
if (!exists("conn")) source(file.path(SCRIPT_DIR, "01_connect_icu_cdm.R"))

lower_names <- function(df) { names(df) <- tolower(names(df)); df }


##### Cohort 구축 전 data 확인

## care_site_name 전체 분포 확인 (어떤 care_site를 ICU로 간주할지 확인하기 위함)
querySql(conn, sprintf(
  "SELECT cs.care_site_name, COUNT(DISTINCT vd.visit_detail_id) AS n
     FROM %s.visit_detail vd
     JOIN %s.care_site cs ON vd.care_site_id = cs.care_site_id
    GROUP BY cs.care_site_name
    ORDER BY n DESC",
  CDM_SCHEMA, CDM_SCHEMA
), integer64AsNumeric = FALSE) %>%
  lower_names() %>%
  print()


## Edge case. 순수 ER(8870) visit_occurrence가 자체 visit_detail을 갖는지 확인
querySql(conn, sprintf(
  "SELECT vo.visit_concept_id, COUNT(DISTINCT vo.visit_occurrence_id) AS n_visits,
          COUNT(DISTINCT vd.visit_detail_id) AS n_details
     FROM %s.visit_occurrence vo
     LEFT JOIN %s.visit_detail vd ON vd.visit_occurrence_id = vo.visit_occurrence_id
    WHERE vo.visit_concept_id = 8870
    GROUP BY vo.visit_concept_id",
  CDM_SCHEMA, CDM_SCHEMA
), integer64AsNumeric = FALSE) %>%
  lower_names() %>%
  print()


## Edge case. 같은 admission, 같은 날짜에 CXR이 2개 이상 존재하는 경우 확인
##   (step1 은 person·날짜 단위로 2장 이상이면 제외)
querySql(conn, sprintf(
  "SELECT n_cxr_per_day, COUNT(*) AS n_person_visit_day
     FROM (
       SELECT io.person_id, io.visit_occurrence_id, io.image_occurrence_date,
              COUNT(*) AS n_cxr_per_day
         FROM %s.image_occurrence io
        WHERE io.modality_concept_id IN (2128009197, 2128009189)  -- CXR modality 로컬 concept_id
        GROUP BY io.person_id, io.visit_occurrence_id, io.image_occurrence_date
     ) sub
    GROUP BY n_cxr_per_day
    ORDER BY n_cxr_per_day",
  CDM_SCHEMA)) %>%
  lower_names() %>%
  print()


## visit_detail FK 무결성 + care_site 연결률 + end_date NULL률 확인
querySql(conn, sprintf(
  "SELECT
   (SELECT COUNT(*) FROM %1$s.visit_detail) AS n_vd_total,
   (SELECT COUNT(*) FROM %1$s.visit_detail vd
      JOIN %1$s.visit_occurrence vo ON vo.visit_occurrence_id = vd.visit_occurrence_id
   ) AS n_vd_joined_to_vo,
   (SELECT COUNT(*) FROM %1$s.visit_detail WHERE care_site_id IS NULL
   ) AS n_vd_null_caresite,
   (SELECT COUNT(*) FROM %1$s.visit_detail vd
      LEFT JOIN %1$s.care_site cs ON cs.care_site_id = vd.care_site_id
     WHERE cs.care_site_id IS NULL
   ) AS n_vd_caresite_unmatched,
   (SELECT COUNT(*) FROM %1$s.visit_detail WHERE visit_detail_end_datetime IS NULL
   ) AS n_vd_null_enddate",
  CDM_SCHEMA), integer64AsNumeric = FALSE) %>% lower_names() %>% print()


## visit_detail table에 person_id가 존재하는지 확인 (있다면 vo join 필요없음)
querySql(conn, sprintf(
  "SELECT column_name FROM information_schema.columns
  WHERE table_schema = '%s' AND table_name = 'visit_detail'
  ORDER BY ordinal_position",
  CDM_SCHEMA)) %>% lower_names() %>% print()


## care_site 기반 ICU 판정 vs visit_detail_concept_id 32037 교차검증
querySql(conn, sprintf(
  "WITH icu_cs AS (
   SELECT care_site_id FROM %1$s.care_site
    WHERE care_site_name IN (
      'Medical Intensive Care Unit (MICU)',
      'Medical/Surgical Intensive Care Unit (MICU/SICU)',
      'Cardiac Vascular Intensive Care Unit (CVICU)',
      'Surgical Intensive Care Unit (SICU)',
      'Coronary Care Unit (CCU)',
      'Trauma SICU (TSICU)',
      'Neuro Surgical Intensive Care Unit (Neuro SICU)',
      'Intensive Care Unit (ICU)')
 )
 SELECT
   CASE WHEN vd.care_site_id IN (SELECT care_site_id FROM icu_cs)
        THEN 1 ELSE 0 END AS icu_by_caresite,
   CASE WHEN vd.visit_detail_concept_id = 32037 THEN 1 ELSE 0 END AS icu_by_concept,
   COUNT(*) AS n
 FROM %1$s.visit_detail vd
 GROUP BY 1, 2 ORDER BY 1, 2",
  CDM_SCHEMA), integer64AsNumeric = FALSE) %>% lower_names() %>% print()


## care_site_id가 NULL인 visit_detail의 concept 분포 확인
querySql(conn, sprintf(
  "SELECT vd.visit_detail_concept_id, c.concept_name, COUNT(*) AS n
   FROM %1$s.visit_detail vd
   LEFT JOIN %1$s.concept c ON c.concept_id = vd.visit_detail_concept_id
  WHERE vd.care_site_id IS NULL
  GROUP BY vd.visit_detail_concept_id, c.concept_name
  ORDER BY n DESC",
  CDM_SCHEMA), integer64AsNumeric = FALSE) %>%
  lower_names() %>%
  print()


##### Cohort 구축 (2단계에 걸쳐 SQL query 작성 및 cohort 구성)

## cohort_step1
# CTE 흐름: icu_care_sites → vd_flagged → vd_seq → {transfer_events, icu_occupied}
#           → cxr_daily → cxr_eligible → qualifying_events → positive_cohort
#                                     → negative_candidates → negative_cohort
sql <- sprintf(
  "WITH icu_care_sites AS (
   SELECT care_site_id
     FROM %1$s.care_site
    WHERE care_site_name IN (
      'Medical Intensive Care Unit (MICU)',
      'Medical/Surgical Intensive Care Unit (MICU/SICU)',
      'Cardiac Vascular Intensive Care Unit (CVICU)',
      'Surgical Intensive Care Unit (SICU)',
      'Coronary Care Unit (CCU)',
      'Trauma SICU (TSICU)',
      'Neuro Surgical Intensive Care Unit (Neuro SICU)',
      'Intensive Care Unit (ICU)'
    )
 ),
 vd_flagged AS (
   SELECT
     vd.person_id,
     vd.visit_occurrence_id,
     vd.visit_detail_start_datetime,
     CAST(vd.visit_detail_start_datetime AS DATE) AS vd_start_date,
     CAST(vd.visit_detail_end_datetime   AS DATE) AS vd_end_date,
     -- care_site_id NULL → IN 결과 NULL → 0 (non-ICU) 처리
     CASE WHEN vd.care_site_id IN (SELECT care_site_id FROM icu_care_sites)
          THEN 1 ELSE 0 END AS is_icu
   FROM %1$s.visit_detail vd
 ),
 vd_seq AS (
   SELECT v.*,
     LAG(v.is_icu) OVER (
       PARTITION BY v.visit_occurrence_id
       ORDER BY v.visit_detail_start_datetime
     ) AS prev_is_icu
   FROM vd_flagged v
 ),
 transfer_events AS (
   -- 병동/ER -> ICU 전환. 첫 유닛이 ICU(직접 입실)면 prev_is_icu NULL → 전원 사건 아님
   SELECT DISTINCT person_id, vd_start_date AS icu_transfer_date
     FROM vd_seq
    WHERE is_icu = 1 AND prev_is_icu = 0
 ),
 icu_occupied AS (
   -- ICU 재실 구간(시작일~종료일, 종료 NULL 이면 시작일). 이 기간 CXR 제외 → 전원 당일(D0) CXR 도 제외
   SELECT person_id, vd_start_date,
          COALESCE(vd_end_date, vd_start_date) AS vd_end_filled
     FROM vd_seq
    WHERE is_icu = 1
 ),
 cxr_daily AS (
   -- 동일 person·날짜 CXR ≥2장 → index 영상 선택이 모호하여 오분류/선택 편향 방지 위해 제외
   SELECT person_id, image_occurrence_id, image_study_uid,
          image_occurrence_date AS cxr_date,
          COUNT(*) OVER (
            PARTITION BY person_id, image_occurrence_date
          ) AS n_same_day
     FROM %1$s.image_occurrence
    WHERE modality_concept_id IN (2128009197, 2128009189)  -- CXR modality 로컬 concept_id
 ),
 cxr_eligible AS (
   SELECT c.person_id, c.image_occurrence_id, c.image_study_uid, c.cxr_date
     FROM cxr_daily c
    WHERE c.n_same_day = 1
      AND NOT EXISTS (
        SELECT 1 FROM icu_occupied r
         WHERE r.person_id = c.person_id
           AND c.cxr_date BETWEEN r.vd_start_date AND r.vd_end_filled
      )
 ),
 qualifying_events AS (
   SELECT te.person_id, te.icu_transfer_date,
          ce.image_occurrence_id, ce.image_study_uid, ce.cxr_date,
          ROW_NUMBER() OVER (  -- 환자당 최초 qualifying 전원 1건
            PARTITION BY te.person_id ORDER BY te.icu_transfer_date
          ) AS event_seq
     FROM transfer_events te
     JOIN cxr_eligible ce
       ON ce.person_id = te.person_id
      AND (te.icu_transfer_date - ce.cxr_date) = 1  -- 익일 전원: 전원일 - CXR일 = 1 (calendar day 단위)
 ),
 positive_cohort AS (
   SELECT person_id, image_occurrence_id, image_study_uid,
          cxr_date AS index_cxr_date, icu_transfer_date, 1 AS outcome
     FROM qualifying_events
    WHERE event_seq = 1
 ),
 negative_candidates AS (
   -- 음성: 전원 사건 없는 환자의 최초 eligible CXR. 직접 ICU 입실 환자도 음성으로 간주 (의도)
   SELECT ce.person_id, ce.image_occurrence_id, ce.image_study_uid, ce.cxr_date,
          ROW_NUMBER() OVER (
            PARTITION BY ce.person_id ORDER BY ce.cxr_date
          ) AS rn
     FROM cxr_eligible ce
    WHERE NOT EXISTS (
      SELECT 1 FROM transfer_events te WHERE te.person_id = ce.person_id
    )
 ),
 negative_cohort AS (
   SELECT person_id, image_occurrence_id, image_study_uid,
          cxr_date AS index_cxr_date,
          CAST(NULL AS DATE) AS icu_transfer_date, 0 AS outcome
     FROM negative_candidates
    WHERE rn = 1
 )
 SELECT * FROM positive_cohort
 UNION ALL
 SELECT * FROM negative_cohort
 ORDER BY person_id",
  CDM_SCHEMA)

cohort_step1 <- querySql(conn, sql, integer64AsNumeric = FALSE) %>% lower_names()

print(table(cohort_step1$outcome))
print(nrow(cohort_step1) == length(unique(cohort_step1$person_id)))  # person 유일성 검증


## cohort_step2
library(dplyr)

# ---------- 0. 변수 정의 ----------
# lo/hi: 생리적 허용 범위 (입력 오류 제거용 plausibility filter)
var_map_df <- tibble::tribble(
  ~concept_id, ~var_name,     ~lo,   ~hi,
  3000963L, "hgb",          2,     25,
  3023314L, "hct",          8,     70,
  3000905L, "wbc",          0.1,   200,
  3024929L, "plt",          1,     2000,
  3020416L, "rbc",          0.5,   10,
  3009744L, "mchc",         20,    45,
  3012030L, "mch",          10,    50,
  3023599L, "mcv",          50,    140,
  3019897L, "rdw",          8,     40,
  3019550L, "sodium",       100,   180,
  3023103L, "potassium",    1,     10,
  3014576L, "chloride",     60,    150,
  3016293L, "bicarbonate",  5,     50,
  3037278L, "anion_gap",    0,     50,
  3013682L, "bun",          1,     200,
  3016723L, "creatinine",   0.1,   25,
  3008342L, "neut_pct",     0,     100,
  3037511L, "lymph_pct",    0,     100
)

# var_map을 SQL VALUES 리터럴 (concept_id,'var_name',lo,hi)로 변환
vm_values <- paste0("(", var_map_df$concept_id, ",'", var_map_df$var_name, "',",
                    var_map_df$lo, ",", var_map_df$hi, ")", collapse = ",\n     ")

# 동적 pivot 절 생성: long → wide (변수값 + 변수별 lag_days)
pivot_val <- paste0("MAX(CASE WHEN var_name='", var_map_df$var_name,
                    "' THEN val END) AS ", var_map_df$var_name, collapse = ",\n     ")
pivot_lag <- paste0("MAX(CASE WHEN var_name='", var_map_df$var_name,
                    "' THEN lag_days END) AS ", var_map_df$var_name, "_lag", collapse = ",\n     ")
sel_cols <- paste0("mw.", c(var_map_df$var_name,
                            paste0(var_map_df$var_name, "_lag")), collapse = ", ")

# ---------- 1. cohort_step1 인라인 ----------
# cohort_step1을 VALUES로 SQL에 인라인 (temp table 미사용, 대규모 코호트 시 SQL 길이 증가)
ck <- cohort_step1 %>% distinct(person_id, image_occurrence_id, index_cxr_date, outcome)
stopifnot(!anyDuplicated(as.character(ck$person_id)))

co_values <- paste0(
  "(", as.character(ck$person_id), ",",
  as.character(ck$image_occurrence_id), ",",
  "DATE '", format(as.Date(ck$index_cxr_date), "%Y-%m-%d"), "',",
  ck$outcome, ")",
  collapse = ",\n     ")

# ---------- 2. SQL query ----------
sql_step2 <- sprintf(
  "WITH cohort AS (
   SELECT * FROM (VALUES
     %2$s
   ) AS t(person_id, image_occurrence_id, index_cxr_date, outcome)
 ),
 var_map AS (
   SELECT * FROM (VALUES
     %3$s
   ) AS t(concept_id, var_name, lo, hi)
 ),
 meas_win AS (
   -- index CXR일 기준 [D-7, D0] lab. D0 포함 → CXR 이후 채혈값 포함 가능
   SELECT co.person_id,
          vm.var_name,
          m.value_as_number AS val,
          COALESCE(m.measurement_date, CAST(m.measurement_datetime AS DATE)) AS meas_date,
          m.measurement_datetime,
          (co.index_cxr_date
           - COALESCE(m.measurement_date, CAST(m.measurement_datetime AS DATE))) AS lag_days
     FROM cohort co
     JOIN %1$s.measurement m ON m.person_id = co.person_id
     JOIN var_map vm ON vm.concept_id = m.measurement_concept_id
    WHERE m.value_as_number IS NOT NULL
      AND m.value_as_number BETWEEN vm.lo AND vm.hi
      AND COALESCE(m.measurement_date, CAST(m.measurement_datetime AS DATE))
            BETWEEN (co.index_cxr_date - 7) AND co.index_cxr_date
 ),
 meas_latest AS (
   -- DISTINCT ON (PostgreSQL 전용): person·변수별 최신값
   SELECT DISTINCT ON (person_id, var_name)
          person_id, var_name, val, lag_days
     FROM meas_win
    ORDER BY person_id, var_name,
             meas_date DESC, measurement_datetime DESC NULLS LAST
 ),
 meas_wide AS (
   SELECT person_id,
     %4$s,
     %5$s
   FROM meas_latest GROUP BY person_id
 )
 SELECT
   co.person_id, co.image_occurrence_id, co.index_cxr_date, co.outcome,
   (EXTRACT(YEAR FROM co.index_cxr_date) - p.year_of_birth) AS age,  -- 연도 차 근사
   p.gender_concept_id,
   gc.concept_name AS sex,
   io.image_study_uid,
   io.image_series_uid,
   io.local_path,
   io.wadors_uri,
   %6$s
 FROM cohort co
 JOIN %1$s.person p            ON p.person_id  = co.person_id
 LEFT JOIN %1$s.concept gc     ON gc.concept_id = p.gender_concept_id
 LEFT JOIN %1$s.image_occurrence io
        ON io.image_occurrence_id = co.image_occurrence_id
 LEFT JOIN meas_wide mw        ON mw.person_id = co.person_id
 ORDER BY co.person_id",
  CDM_SCHEMA, co_values, vm_values, pivot_val, pivot_lag, sel_cols)

cohort_step2 <- querySql(conn, sql_step2, integer64AsNumeric = FALSE) %>% lower_names()


sum(is.na(cohort_step2$local_path))
head(cohort_step2$local_path, 5)

nrow(cohort_step2); nrow(cohort_step1)
table(cohort_step2$outcome, useNA = "ifany")
table(cohort_step2$sex, useNA = "ifany")


# 변수별 결측률
miss <- sapply(cohort_step2[var_map_df$var_name], function(x) mean(is.na(x)))
sort(miss)

# 환자별 확보한 변수 개수 분포 — complete-case 판단
n_obs <- rowSums(!is.na(cohort_step2[var_map_df$var_name]))
table(n_obs)

# outcome별 결측 차이 — informative missingness 확인
tapply(n_obs, cohort_step2$outcome, summary)


summary(cohort_step2[c("age", var_map_df$var_name)])
summary(cohort_step2[paste0(var_map_df$var_name, "_lag")])


# --- 파일 저장 (코드 파일과 같은 폴더) ----------------------------
out_csv <- file.path(SCRIPT_DIR, "icu_cohort.csv")
write.csv(cohort_step2, out_csv, row.names = FALSE)

cat("\n[완료] 코호트 저장 =>", out_csv, "\n")
cat("코호트 미리보기:\n")
print(head(cohort_step2, 5))