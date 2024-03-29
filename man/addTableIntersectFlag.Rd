% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addTableIntersect.R
\name{addTableIntersectFlag}
\alias{addTableIntersectFlag}
\title{Compute a flag intersect with an omop table.}
\usage{
addTableIntersectFlag(
  x,
  tableName,
  indexDate = "cohort_start_date",
  censorDate = NULL,
  window = list(c(0, Inf)),
  overlap = TRUE,
  nameStyle = "{table_name}_{window_name}"
)
}
\arguments{
\item{x}{Table with individuals in the cdm.}

\item{tableName}{Name of the table to intersect with. Options:
visit_occurrence, condition_occurrence, drug_exposure, procedure_occurrence,
device_exposure, measurement, observation, drug_era, condition_era, specimen.}

\item{indexDate}{Variable in x that contains the date to compute the
intersection.}

\item{censorDate}{whether to censor overlap events at a specific date
or a column date of x.}

\item{window}{window to consider events in.}

\item{overlap}{Whether to consider end date or only end date for the
intersection..}

\item{nameStyle}{naming of the added column or columns, should include
required parameters}
}
\value{
table with added columns with intersect information.
}
\description{
Compute a flag intersect with an omop table.
}
\examples{
\donttest{
cdm <- mockPatientProfiles()

cdm$cohort1 \%>\%
  addTableIntersectFlag(tableName = "visit_occurrence")
}

}
