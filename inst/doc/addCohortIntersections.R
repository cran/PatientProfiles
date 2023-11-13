## ----eval=FALSE---------------------------------------------------------------
#  library(DBI)
#  library(CDMConnector)
#  
#  # The input arguments provided are for illustrative purposes only and do not provide access to any database.
#  
#  con <- DBI::dbConnect(RPostgres::Postgres(),
#    dbname = "omop_cdm",
#    host = "10.80.192.00",
#    user = "user_name",
#    password = "user_pasword"
#  )
#  
#  cdm <- CDMConnector::cdm_from_con(con,
#    cdm_schema = "main",
#    write_schema = "main",
#    cohort_tables = "cohort_example"
#  )

## ----message= FALSE, warning=FALSE--------------------------------------------
library(PatientProfiles)
library(duckdb)
library(dplyr)

cdm <- mockPatientProfiles(
  patient_size = 1000,
  drug_exposure_size = 1000
)

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1 %>%
  glimpse()

cdm$cohort2 %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1WashOut <- cdm$cohort1 %>%
  addCohortIntersectFlag(
    targetCohortTable = "cohort2",
    window = list(c(-180, -1)),
    targetCohortId = 1,
  ) %>%
  filter(cohort_1_m180_to_m1 == 0)

cdm$cohort1WashOut %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1StrokeCounts <- cdm$cohort1 %>%
  addCohortIntersectCount(
    targetCohortTable = "cohort2",
    window = list(c(-Inf, -366), c(-365, -181), c(-180, -1)),
    targetCohortId = 1
  )

cdm$cohort1StrokeCounts %>%
  glimpse()

## -----------------------------------------------------------------------------
# This will be our "main" cohort
cohort1 <- dplyr::tibble(
  cohort_definition_id = 1,
  subject_id = c("1", "2"),
  cohort_start_date = c(
    as.Date("2010-03-01"),
    as.Date("2012-03-01")
  ),
  cohort_end_date = c(
    as.Date("2015-01-01"),
    as.Date("2016-03-01")
  )
)

# This is the cohort with the events we are interested in
cohort2 <- dplyr::tibble(
  cohort_definition_id = 1,
  subject_id = c("1", "1", "1", "2"),
  cohort_start_date = c(
    as.Date("2010-03-03"),
    as.Date("2010-02-27"),
    as.Date("2010-01-25"),
    as.Date("2013-01-03")
  ),
  cohort_end_date = c(
    as.Date("2010-03-03"),
    as.Date("2010-02-27"),
    as.Date("2012-03-25"),
    as.Date("2013-01-03")
  )
)

cdm <- mockPatientProfiles(
  cohort1 = cohort1,
  cohort2 = cohort2
)

cdm$cohort1 <- cdm$cohort1 %>% addCohortIntersectCount(cdm, targetCohortTable = "cohort2", window = list(c(-30, -1)))
cdm$cohort1

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1 <- cdm$cohort1 %>% addCohortIntersectCount(cdm, targetCohortTable = "cohort2", window = list(c(-30, -1)), targetEndDate = "cohort_start_date")
cdm$cohort1

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1TimeTo <- cdm$cohort1 %>%
  addCohortIntersectDays(
    targetCohortTable = "cohort2",
    targetCohortId = 1,
    order = "first"
  )

cdm$cohort1TimeTo %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1NextEvent <- cdm$cohort1 %>%
  addCohortIntersectDate(
    targetCohortTable = "cohort2",
    order = "first",
    targetCohortId = 1,
    window = c(1, Inf)
  )

cdm$cohort1NextEvent %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1CohortIntersect <- cdm$cohort1 %>%
  addCohortIntersect(
    targetCohortTable = "cohort2",
    order = "first",
    targetCohortId = 1,
    window = c(1, Inf)
  )
cdm$cohort1CohortIntersect %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1CohortIntersect <- cdm$cohort1 %>%
  addCohortIntersect(
    targetCohortTable = "cohort2",
    order = "first",
    targetCohortId = 1,
    window = c(1, Inf),
    flag = TRUE,
    count = TRUE,
    date = FALSE,
    days = FALSE
  )
cdm$cohort1CohortIntersect %>%
  glimpse()

