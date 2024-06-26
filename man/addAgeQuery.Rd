% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addDemographicsQuery.R
\name{addAgeQuery}
\alias{addAgeQuery}
\title{Query to add the age of the individuals at a certain date}
\usage{
addAgeQuery(
  x,
  indexDate = "cohort_start_date",
  ageName = "age",
  ageGroup = NULL,
  ageMissingMonth = 1,
  ageMissingDay = 1,
  ageImposeMonth = FALSE,
  ageImposeDay = FALSE,
  missingAgeGroupValue = "None"
)
}
\arguments{
\item{x}{Table with individuals in the cdm.}

\item{indexDate}{Variable in x that contains the date to compute the age.}

\item{ageName}{Name of the new column that contains age.}

\item{ageGroup}{List of age groups to be added.}

\item{ageMissingMonth}{Month of the year assigned to individuals with missing
month of birth. By default: 1.}

\item{ageMissingDay}{day of the month assigned to individuals with missing
day of birth. By default: 1.}

\item{ageImposeMonth}{Whether the month of the date of birth will be
considered as missing for all the individuals.}

\item{ageImposeDay}{Whether the day of the date of birth will be considered
as missing for all the individuals.}

\item{missingAgeGroupValue}{Value to include if missing age.}
}
\value{
tibble with the age column added.
}
\description{
`r lifecycle::badge("experimental")`
Same as `addAge()`, except query is not computed to a table.
}
\examples{
\donttest{
cdm <- mockPatientProfiles()

cdm$cohort1 |>
  addAgeQuery()
mockDisconnect(cdm = cdm)
}
}
