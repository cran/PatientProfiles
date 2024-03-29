% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gtResult.R
\name{gtCharacteristics}
\alias{gtCharacteristics}
\title{Create a gt table from a summarisedCharacteristics object.}
\usage{
gtCharacteristics(
  summarisedCharacteristics,
  pivotWide = c("CDM Name", "Group", "Strata"),
  format = c(`N (\%)` = "count (percentage\%)", "median [min; q25 - q75; max]",
    "mean (sd)", "median [q25 - q75]", N = "count"),
  keepNotFormatted = TRUE,
  decimals = c(default = 0),
  decimalMark = ".",
  bigMark = ","
)
}
\arguments{
\item{summarisedCharacteristics}{Summary characteristics long table.}

\item{pivotWide}{variables to pivot wide}

\item{format}{formats and labels to use}

\item{keepNotFormatted}{Whether to keep not formatted estimate types}

\item{decimals}{Decimals per estimate_type}

\item{decimalMark}{decimal mark}

\item{bigMark}{big mark}
}
\value{
New table in gt format
}
\description{
`r lifecycle::badge("deprecated")`
}
\examples{
\donttest{
library(PatientProfiles)

cdm <- mockPatientProfiles()

summariseCharacteristics(
  cohort = cdm$cohort1,
  ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)),
  tableIntersect = list(
    "Visits" = list(
      tableName = "visit_occurrence", value = "count", window = c(-365, 0)
    )
  ),
  cohortIntersect = list(
    "Medications" = list(
      targetCohortTable = "cohort2", value = "flag", window = c(-365, 0)
    )
  )
)
}
}
