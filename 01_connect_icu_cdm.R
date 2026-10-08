# --- 0. 패키지 (없으면 설치) -----------------------------------------
# 순수한 R 환경을 가정한 후 미설치된 패키지 설치.
#   DatabaseConnector: OHDSI 표준 DB 커넥터 (JDBC 로 DB에 접속)
#   dplyr: 결과를 파이프(%>%)로 처리

ensure <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}
for (p in c("DatabaseConnector", "dplyr")) ensure(p)

# --- 1. CDM 접속 정보 ------------------------------------------------
# 병원 CDM 은 DB 안에 존재 → "CDM 연결" = DB 커넥션
# 주의: 비밀번호는 환경변수로 관리
#   터미널: export CDM_DB_PASSWORD='...'
#   R: Sys.setenv(CDM_DB_PASSWORD = "...")

dbPassword <- Sys.getenv("CDM_DB_PASSWORD")
if (!nzchar(dbPassword)) stop("CDM_DB_PASSWORD 를 먼저 설정할 것.")

# Declare configurations for connection to DB via OHDSI
connectionDetails <- createConnectionDetails(
  dbms         = "postgresql",
  server       = "10.60.0.2/mimiciv_cdm",
  port         = 15432,
  user         = "cdm_user",
  password     = dbPassword,
  pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER", "/opt/ohdsi/jdbc/postgresql")
)

CDM_SCHEMA <- "cdm"

conn <- connect(connectionDetails)
cat("CDM 스키마:", CDM_SCHEMA, "\n\n")

# --- 2. 연결확인: 테이블 확인 ----------------------------------------
# CDM 표준 테이블 이름: person, condition_occurrence, concept ...
# image_occurrence: 영상을 CDM 에 얹는 확장 테이블

# Sampled examples of available CDM tables
cat("사용 가능한 CDM 테이블:\n")
querySql(conn, sprintf(
  "SELECT table_name FROM information_schema.tables
    WHERE table_schema = '%s' ORDER BY table_name", CDM_SCHEMA)) %>%
  pull(1) %>%
  print()

# --- 3. 연결 확인 (sanity check) -------------------------------------

cat("\n[연결 확인]\n")
cat("\nperson 테이블 미리보기 (5행):\n")
querySql(conn, sprintf(
  "SELECT CAST(person_id AS VARCHAR) AS person_id,
          person_source_value, year_of_birth,
          gender_concept_id, gender_source_value
     FROM %s.person LIMIT 5", CDM_SCHEMA)) %>%
  print()

# 핵심 포인트:
#   person_source_value 컬럼 = MIMIC 의 subject_id (환자 식별자)
#   의료 영상과 코호트를 잇는 열쇠

cat("\n연결 완료. 다음 단계(02_build_icu_cohort.R)에서 코호트 구축 예정.\n")


##### Demo SQL query 1
cohort <- querySql(conn, sprintf(
  "WITH index_admission AS (
   -- 환자별 최초 inpatient 입원 선정 (outpatient-only 환자는 여기서 제외)
   SELECT
     vo.person_id,
     vo.visit_occurrence_id,
     vo.visit_start_datetime,
     ROW_NUMBER() OVER (
       PARTITION BY vo.person_id ORDER BY vo.visit_start_datetime, vo.visit_occurrence_id
     ) AS rn
   FROM %s.visit_occurrence vo
   WHERE vo.visit_concept_id IN (9201, 262)   -- Inpatient Visit / ER-to-Inpatient
 ),
 first_admission AS (
   SELECT person_id, visit_occurrence_id, visit_start_datetime AS admission_time
   FROM index_admission
   WHERE rn = 1
 ),
 vd_seq AS (
   -- 최초 입원 내 유닛 이동 시퀀스 + ICU 전원 여부 확인
   SELECT
     fa.person_id,
     fa.visit_occurrence_id,
     fa.admission_time,
     vd.visit_detail_start_datetime,
     CASE WHEN cs.care_site_name ILIKE '%%intensive care unit%%' THEN 1 ELSE 0 END AS is_icu,
     ROW_NUMBER() OVER (
       PARTITION BY fa.visit_occurrence_id ORDER BY vd.visit_detail_start_datetime
     ) AS unit_seq,
     LAG(CASE WHEN cs.care_site_name ILIKE '%%intensive care unit%%' THEN 1 ELSE 0 END) OVER (
       PARTITION BY fa.visit_occurrence_id ORDER BY vd.visit_detail_start_datetime
     ) AS prev_is_icu
   FROM first_admission fa
   JOIN %s.visit_detail vd ON fa.visit_occurrence_id = vd.visit_occurrence_id
   JOIN %s.care_site cs    ON vd.care_site_id = cs.care_site_id
 ),
 direct_icu_admission AS (
   -- 최초 유닛부터 ICU인 환자 -> total에서 제외
   SELECT DISTINCT visit_occurrence_id
   FROM vd_seq
   WHERE unit_seq = 1 AND is_icu = 1
 ),
 first_ward_to_icu AS (
   -- 병동(non-ICU) -> ICU 전환 이벤트 중 '가장 이른' 이벤트만 추출
   SELECT
     person_id, visit_occurrence_id, admission_time, visit_detail_start_datetime AS icu_transfer_time,
     ROW_NUMBER() OVER (
       PARTITION BY visit_occurrence_id ORDER BY visit_detail_start_datetime
     ) AS transfer_seq
   FROM vd_seq
   WHERE is_icu = 1 AND prev_is_icu = 0
 ),
 first_transfer_only AS (
   SELECT person_id, visit_occurrence_id, admission_time, icu_transfer_time
   FROM first_ward_to_icu
   WHERE transfer_seq = 1
 ),
 cohort AS (
   SELECT
     fa.person_id,
     fa.visit_occurrence_id,
     fa.admission_time,
     ft.icu_transfer_time,
     EXTRACT(EPOCH FROM (ft.icu_transfer_time - fa.admission_time)) / 86400.0 AS days_to_transfer,
     CASE
       WHEN ft.icu_transfer_time IS NULL THEN 'ward_only'
       WHEN EXTRACT(EPOCH FROM (ft.icu_transfer_time - fa.admission_time)) / 86400.0 <= 2
         THEN 'early_icu_transfer'
       ELSE 'late_icu_transfer'
     END AS category
   FROM first_admission fa
   LEFT JOIN first_transfer_only ft ON fa.visit_occurrence_id = ft.visit_occurrence_id
   WHERE fa.visit_occurrence_id NOT IN (SELECT visit_occurrence_id FROM direct_icu_admission)
 )
 SELECT *
 FROM cohort
 WHERE category IN ('ward_only', 'early_icu_transfer')
 ORDER BY person_id",
  CDM_SCHEMA, CDM_SCHEMA, CDM_SCHEMA
), integer64AsNumeric = FALSE)


##### Demo SQL query 2
querySql(conn, sprintf(
  "WITH index_admission AS (
   SELECT
     vo.person_id,
     vo.visit_occurrence_id,
     vo.visit_start_datetime,
     ROW_NUMBER() OVER (
       PARTITION BY vo.person_id ORDER BY vo.visit_start_datetime, vo.visit_occurrence_id
     ) AS rn
   FROM %s.visit_occurrence vo
   WHERE vo.visit_concept_id IN (9201, 262)
 ),
 first_admission AS (
   SELECT person_id, visit_occurrence_id, visit_start_datetime AS admission_time
   FROM index_admission
   WHERE rn = 1
 ),
 vd_seq AS (
   SELECT
     fa.person_id,
     fa.visit_occurrence_id,
     fa.admission_time,
     vd.visit_detail_start_datetime,
     CASE WHEN cs.care_site_name ILIKE '%%intensive care unit%%' THEN 1 ELSE 0 END AS is_icu,
     ROW_NUMBER() OVER (
       PARTITION BY fa.visit_occurrence_id ORDER BY vd.visit_detail_start_datetime
     ) AS unit_seq,
     LAG(CASE WHEN cs.care_site_name ILIKE '%%intensive care unit%%' THEN 1 ELSE 0 END) OVER (
       PARTITION BY fa.visit_occurrence_id ORDER BY vd.visit_detail_start_datetime
     ) AS prev_is_icu
   FROM first_admission fa
   JOIN %s.visit_detail vd ON fa.visit_occurrence_id = vd.visit_occurrence_id
   JOIN %s.care_site cs    ON vd.care_site_id = cs.care_site_id
 ),
 direct_icu_admission AS (
   SELECT DISTINCT visit_occurrence_id
   FROM vd_seq
   WHERE unit_seq = 1 AND is_icu = 1
 ),
 first_ward_to_icu AS (
   SELECT
     person_id, visit_occurrence_id, admission_time, visit_detail_start_datetime AS icu_transfer_time,
     ROW_NUMBER() OVER (
       PARTITION BY visit_occurrence_id ORDER BY visit_detail_start_datetime
     ) AS transfer_seq
   FROM vd_seq
   WHERE is_icu = 1 AND prev_is_icu = 0
 ),
 first_transfer_only AS (
   SELECT person_id, visit_occurrence_id, admission_time, icu_transfer_time
   FROM first_ward_to_icu
   WHERE transfer_seq = 1
 ),
 cohort AS (
   SELECT
     fa.person_id,
     fa.visit_occurrence_id,
     fa.admission_time,
     ft.icu_transfer_time,
     EXTRACT(EPOCH FROM (ft.icu_transfer_time - fa.admission_time)) / 86400.0 AS days_to_transfer,
     CASE
       WHEN ft.icu_transfer_time IS NULL THEN 'ward_only'
       WHEN EXTRACT(EPOCH FROM (ft.icu_transfer_time - fa.admission_time)) / 86400.0 <= 2
         THEN 'early_icu_transfer'
       ELSE 'late_icu_transfer'
     END AS category
   FROM first_admission fa
   LEFT JOIN first_transfer_only ft ON fa.visit_occurrence_id = ft.visit_occurrence_id
   WHERE fa.visit_occurrence_id NOT IN (SELECT visit_occurrence_id FROM direct_icu_admission)
 )
 SELECT category, COUNT(*) AS n_patients
 FROM cohort
 WHERE category IN ('ward_only', 'early_icu_transfer')
 GROUP BY category",
  CDM_SCHEMA, CDM_SCHEMA, CDM_SCHEMA
), integer64AsNumeric = FALSE)