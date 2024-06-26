% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addDemographics.R
\name{addSex}
\alias{addSex}
\title{Compute the sex of the individuals}
\usage{
addSex(x, sexName = "sex", missingSexValue = "None", name = NULL)
}
\arguments{
\item{x}{Table with individuals in the cdm.}

\item{sexName}{name of the new column to be added.}

\item{missingSexValue}{Value to include if missing sex.}

\item{name}{Name of the new table, if NULL a temporary table is returned.}
}
\value{
table x with the added column with sex information.
}
\description{
Compute the sex of the individuals
}
\examples{
\donttest{
cdm <- mockPatientProfiles()
cdm$cohort1 \%>\%
  addSex()
mockDisconnect(cdm = cdm)
}

}
