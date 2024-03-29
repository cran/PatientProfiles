% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addTableIntersect.R
\name{addTableIntersectField}
\alias{addTableIntersectField}
\title{Obtain a column's value of the intersect with an omop table,}
\usage{
addTableIntersectField(
  x,
  tableName,
  field,
  indexDate = "cohort_start_date",
  censorDate = NULL,
  window = list(c(0, Inf)),
  targetDate = startDateColumn(tableName),
  order = "first",
  nameStyle = "{table_name}_{extra_value}_{window_name}"
)
}
\arguments{
\item{x}{Table with individuals in the cdm.}

\item{tableName}{Name of the table to intersect with. Options:
visit_occurrence, condition_occurrence, drug_exposure, procedure_occurrence,
device_exposure, measurement, observation, drug_era, condition_era, specimen.}

\item{field}{Other columns from the table to intersect.}

\item{indexDate}{Variable in x that contains the date to compute the
intersection.}

\item{censorDate}{whether to censor overlap events at a specific date
or a column date of x.}

\item{window}{window to consider events in.}

\item{targetDate}{Target date in tableName.}

\item{order}{which record is considered in case of multiple records (only
required for date and days options).}

\item{nameStyle}{naming of the added column or columns, should include
required parameters}
}
\value{
table with added columns with intersect information.
}
\description{
Obtain a column's value of the intersect with an omop table,
}
\examples{
\donttest{
cdm <- mockPatientProfiles()

cdm$cohort1 \%>\%
  addTableIntersectField(
    tableName = "visit_occurrence",
    field = "visit_concept_id",
    order = "last",
    window = c(-Inf, -1)
  )
}

}
