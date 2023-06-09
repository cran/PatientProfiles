% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addIntersect.R
\name{getConceptName}
\alias{getConceptName}
\title{Get the name of the concept_id column for a certain table in the cdm}
\usage{
getConceptName(tableName)
}
\arguments{
\item{tableName}{Name of the table}
}
\value{
Name of the concept_id column in that table
}
\description{
Get the name of the concept_id column for a certain table in the cdm
}
\examples{
\donttest{
library(PatientProfiles)
getConceptName("condition_occurrence")
}

}
