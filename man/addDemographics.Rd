% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addDemographics.R
\name{addDemographics}
\alias{addDemographics}
\title{Compute demographic characteristics at a certain date}
\usage{
addDemographics(
  x,
  cdm,
  indexDate = "cohort_start_date",
  age = TRUE,
  ageName = "age",
  ageDefaultMonth = 1,
  ageDefaultDay = 1,
  ageImposeMonth = FALSE,
  ageImposeDay = FALSE,
  ageGroup = NULL,
  sex = TRUE,
  sexName = "sex",
  priorHistory = TRUE,
  priorHistoryName = "prior_history",
  futureObservation = TRUE,
  futureObservationName = "future_observation",
  tablePrefix = NULL
)
}
\arguments{
\item{x}{Table with individuals in the cdm}

\item{cdm}{Object that contains a cdm reference. Use CDMConnector to obtain a
cdm reference.}

\item{indexDate}{Variable in x that contains the date to compute the
demographics characteristics.}

\item{age}{TRUE or FALSE. If TRUE, age will be calculated relative to
indexDate}

\item{ageName}{Age variable name}

\item{ageDefaultMonth}{Month of the year assigned to individuals with missing
month of birth.}

\item{ageDefaultDay}{day of the month assigned to individuals
with missing day of birth.}

\item{ageImposeMonth}{TRUE or FALSE. Whether the month of the date of birth
will be considered as missing for all the individuals.}

\item{ageImposeDay}{TRUE or FALSE. Whether the day of the date of birth
will be considered as missing for all the individuals.}

\item{ageGroup}{if not NULL, a list of ageGroup vectors}

\item{sex}{TRUE or FALSE. If TRUE, sex will be identified}

\item{sexName}{Sex variable name}

\item{priorHistory}{TRUE or FALSE. If TRUE, days of between the start
of the current observation period and the indexDate will be calculated}

\item{priorHistoryName}{Prior history variable name}

\item{futureObservation}{TRUE or FALSE. If TRUE, days between the
indexDate and the end of the current observation period will be
calculated}

\item{futureObservationName}{Future observation variable name}

\item{tablePrefix}{The stem for the permanent tables that will
be created. If NULL, temporary tables will be used throughout.}
}
\value{
cohort table with the added demographic information columns
}
\description{
Compute demographic characteristics at a certain date
}
\examples{
\donttest{
library(PatientProfiles)
cdm <- mockPatientProfiles()
cdm$cohort1 \%>\% addDemographics(cdm)
}

}
