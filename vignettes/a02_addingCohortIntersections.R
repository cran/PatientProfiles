## ----message= FALSE, warning=FALSE--------------------------------------------
library(CDMConnector)
library(PatientProfiles)
library(duckdb)
library(dplyr)
library(ggplot2)

cdm <- mockPatientProfiles(
  patient_size = 1000,
  drug_exposure_size = 1000
)

# In this mock dataset there are the following cohort tables:
cdm$cohort1 %>%
  glimpse()
settings(cdm$cohort1)

cdm$cohort2 %>%
  glimpse()
settings(cdm$cohort2)

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1 %>%
  addCohortIntersectFlag(indexDate = "cohort_start_date",
    targetCohortTable = "cohort2",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    window = list(c(-180, 180)) 
  ) |> 
  glimpse()

cdm$cohort1 %>%
  addCohortIntersectCount(indexDate = "cohort_start_date",
    targetCohortTable = "cohort2",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    window = list(c(-180, 180)) 
  ) |> 
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1 %>%
  addCohortIntersectDate(indexDate = "cohort_start_date",
    targetCohortTable = "cohort2",
    targetDate = "cohort_start_date",
    window = list(c(-180, 180)), 
    order = "first" 
  ) |> 
  glimpse()

cdm$cohort1 %>%
  addCohortIntersectDays(indexDate = "cohort_start_date",
    targetCohortTable = "cohort2",
    targetDate = "cohort_start_date",
    window = list(c(-180, 180)), 
    order = "first"  
  ) |> 
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
common_cold <- dplyr::tibble(
    cohort_definition_id = 1,
    subject_id = 1,
    cohort_start_date = as.Date("2020-02-01"),
    cohort_end_date = as.Date("2020-02-15")
  )

aspirin <- dplyr::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c(1, 1),
    cohort_start_date = as.Date(c("2020-01-01", "2020-02-10")),
    cohort_end_date = as.Date(c("2020-01-28", "2020-03-15"))
  )
 
bind_rows(common_cold |> mutate(cohort = "common cold"),
          aspirin |> mutate(cohort = "aspirin")) |> 
  mutate(record = as.character(row_number())) |>
  ggplot() +
geom_segment(aes(x = cohort_start_date, 
                 y = cohort,
                 xend = cohort_end_date, 
                 yend = cohort, col = cohort, fill = cohort),
            size = 4.5, alpha = .5) +
  geom_point(aes(x = cohort_start_date, y = cohort, color = cohort), size = 4) +
  geom_point(aes(x = cohort_end_date, y = cohort, color = cohort), size = 4) +
  ylab("") +
  xlab("")+
  theme_minimal() +
  theme(legend.position = "none")

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm <- mockPatientProfiles(
  list(
    con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
    write_schema = "main"
  ),
  cohort1 = common_cold,
  cohort2 = aspirin,
  patient_size = 2
)

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1 %>%
  addCohortIntersectFlag(
    targetCohortTable = "cohort2",
    indexDate = "cohort_start_date", 
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    window = list(c(0, 0)), 
  ) |> 
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1 %>%
  addCohortIntersectFlag(
    targetCohortTable = "cohort2",
    indexDate = "cohort_end_date", 
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    window = list(c(0, 0)), 
  ) |> 
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1 %>%
  addCohortIntersectFlag(
    targetCohortTable = "cohort2",
    indexDate = "cohort_start_date", 
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    window = list(c(-90, 90)), 
  ) |> 
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1 %>%
  addCohortIntersectCount(
    targetCohortTable = "cohort2",
    indexDate = "cohort_start_date", 
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    window = list(c(-90, 90)), 
  ) |> 
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1 %>%
  addCohortIntersectDate(
    targetCohortTable = "cohort2",
    indexDate = "cohort_start_date", 
    targetDate = "cohort_start_date",
    window = list(c(-90, 90)), 
    order = "first"
  ) |> 
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1 %>%
  addCohortIntersectDate(
    targetCohortTable = "cohort2",
    indexDate = "cohort_start_date", 
    targetDate = "cohort_start_date",
    window = list(c(-90, 90)), 
    order = "last"
  ) |> 
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1 %>%
  addCohortIntersectDate(
    targetCohortTable = "cohort2",
    indexDate = "cohort_start_date", 
    targetDate = "cohort_start_date",
    window = list(c(-90, 90)), 
    order = "last",
    nameStyle = "{cohort_name}_{window_name}"
  ) |> 
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1 %>%
  addCohortIntersectDate(
    targetCohortTable = "cohort2",
    indexDate = "cohort_start_date", 
    targetDate = "cohort_start_date",
    window = list(c(-90, 90)), 
    order = "last",
    nameStyle = "my_new_variable"
  ) |> 
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1 |> 
  addCohortIntersectDate(
    targetCohortTable = "cohort2",
    indexDate = "cohort_start_date", 
    targetDate = "cohort_start_date",
    window = list(c(-90, 90)), 
    order = "last",
    nameStyle = "{cohort_name}_{window_name}_{value}"
  ) |> 
  addCohortIntersectDays(
    targetCohortTable = "cohort2",
    indexDate = "cohort_start_date", 
    targetDate = "cohort_start_date",
    window = list(c(-90, 90)), 
    order = "last",
    nameStyle = "{cohort_name}_{window_name}_{value}"
  ) |> 
  glimpse()

