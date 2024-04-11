## ----message= FALSE, warning=FALSE--------------------------------------------
library(PatientProfiles)
library(duckdb)
library(dplyr)

cdm <- mockPatientProfiles(
  patient_size = 10000,
  drug_exposure_size = 10000
)

cdm$person %>% 
  dplyr::glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$observation_period %>% 
  dplyr::glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$condition_occurrence %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$condition_occurrence <- cdm$condition_occurrence %>%
  addAge(indexDate = "condition_start_date")

cdm$condition_occurrence %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$condition_occurrence <- cdm$condition_occurrence %>%
  addAge(
    indexDate = "condition_start_date",
    ageGroup = list(
        "0 to 17" = c(0, 17),
        "18 to 65" = c(18, 65),
        ">= 66" = c(66, Inf)))

cdm$condition_occurrence %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$condition_occurrence <- cdm$condition_occurrence %>%
  addAge(indexDate = "condition_start_date", 
         ageName = "age_from_year_of_birth", 
         ageDefaultMonth = 1,
         ageDefaultDay = 1,
         ageImposeMonth = TRUE, 
         ageImposeDay = TRUE)

cdm$condition_occurrence %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$condition_occurrence <- cdm$condition_occurrence %>%
  addSex()

cdm$condition_occurrence %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$condition_occurrence <- cdm$condition_occurrence %>%
  addInObservation(indexDate = "condition_start_date") %>% 
  addPriorObservation(indexDate = "condition_start_date") %>% 
  addFutureObservation(indexDate = "condition_start_date")

cdm$condition_occurrence %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1 %>%
  glimpse()

cdm$cohort1 <- cdm$cohort1 %>%
  addAge(ageGroup = list(
        "0 to 17" = c(0, 17),
        "18 to 65" = c(18, 65),
        ">= 66" = c(66, Inf))) %>% 
  addSex() %>% 
  addInObservation() %>%
  addPriorObservation() %>%
  addFutureObservation()

cdm$cohort1 %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort2 %>%
  glimpse()

tictoc::tic()
cdm$cohort2 %>%
  addAge(ageGroup = list(
        "0 to 17" = c(0, 17),
        "18 to 65" = c(18, 65),
        ">= 66" = c(66, Inf))) %>% 
  addSex() %>% 
  addInObservation() %>%
  addPriorObservation() %>%
  addFutureObservation()
tictoc::toc()

tictoc::tic()
cdm$cohort2 %>%
  addDemographics(
    age = TRUE,
    ageName = "age",
    ageGroup = list(
        "0 to 17" = c(0, 17),
        "18 to 65" = c(18, 65),
        ">= 66" = c(66, Inf)),
    sex = TRUE,
    sexName = "sex",
    priorObservation = TRUE,
    priorObservationName = "prior_observation",
    futureObservation = FALSE,
  ) %>%
  glimpse()
tictoc::toc()

