% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filterInObservation.R
\name{filterInObservation}
\alias{filterInObservation}
\title{Filter the rows of a `cdm_table` to the ones in observation that `indexDate`
is in observation.}
\usage{
filterInObservation(x, indexDate)
}
\arguments{
\item{x}{A `cdm_table` object.}

\item{indexDate}{Name of a column of x that is a date.}
}
\value{
A `cdm_table` that is a subset of the original table.
}
\description{
Filter the rows of a `cdm_table` to the ones in observation that `indexDate`
is in observation.
}
\examples{
\dontrun{
con <- duckdb::dbConnect(duckdb::duckdb(CDMConnector::eunomiaDir()))
cdm <- CDMConnector::cdmFromCon(
  con = con, cdmSchema = "main", writeSchema = "main"
)

cdm$condition_occurrence |>
  filterInObservation(indexDate = "condition_start_date") |>
  dplyr::compute()
}

}
