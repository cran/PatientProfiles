% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addDemographics.R
\name{addAge}
\alias{addAge}
\title{Compute the age of the individuals at a certain date}
\usage{
addAge(
  x,
  cdm,
  indexDate = "cohort_start_date",
  ageName = "age",
  ageGroup = NULL,
  ageDefaultMonth = 1,
  ageDefaultDay = 1,
  ageImposeMonth = FALSE,
  ageImposeDay = FALSE,
  tablePrefix = NULL
)
}
\arguments{
\item{x}{Table with individuals in the cdm.}

\item{cdm}{Object that contains a cdm reference. Use CDMConnector to obtain a
cdm reference.}

\item{indexDate}{Variable in x that contains the date to compute the age.}

\item{ageName}{Name of the new column that contains age.}

\item{ageGroup}{List of age groups to be added.}

\item{ageDefaultMonth}{Month of the year assigned to individuals with missing
month of birth. By default: 1.}

\item{ageDefaultDay}{day of the month assigned to individuals with missing
day of birth. By default: 1.}

\item{ageImposeMonth}{Whether the month of the date of birth will be
considered as missing for all the individuals.}

\item{ageImposeDay}{Whether the day of the date of birth will be considered
as missing for all the individuals.}

\item{tablePrefix}{The stem for the permanent tables that will
be created. If NULL, temporary tables will be used throughout.}
}
\value{
tibble with the age column added
}
\description{
Compute the age of the individuals at a certain date
}
\examples{
\donttest{
library(DBI)
library(duckdb)
library(PatientProfiles)
cohort1 <- dplyr::tibble(
  cohort_definition_id = c("1", "1", "1"),
  subject_id = c("1", "2", "3"),
  cohort_start_date = c(
    as.Date("2010-01-01"), as.Date("2010-01-01"), as.Date("2010-01-01")
  ),
  cohort_end_date = c(
    as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2018-01-01")
  )
)

person <- dplyr::tibble(
  person_id = c("1", "2", "3"),
  gender_concept_id = c("8507", "8532", "8507"),
  year_of_birth = c(2000, 1995, NA),
  month_of_birth = c(NA, 07, 08),
  day_of_birth = c(01, 25, 03)
)
cdm <- mockPatientProfiles(person = person, cohort1 = cohort1)
addAge(x = cdm[["cohort1"]], cdm = cdm)
}
}
