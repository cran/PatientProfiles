# Copyright 2024 DARWIN EU (C)
#
# This file is part of PatientProfiles
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' It creates columns to indicate the presence of cohorts
#'
#' @param x Table with individuals in the cdm.
#' @param targetCohortTable name of the cohort that we want to check for overlap.
#' @param targetCohortId vector of cohort definition ids to include.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x.
#' @param targetStartDate date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence).
#' @param targetEndDate date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence).
#' @param window window to consider events of.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters.
#' @param name Name of the new table, if NULL a temporary table is returned.
#'
#' @return table with added columns with overlap information.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 |>
#'   addCohortIntersectFlag(
#'     targetCohortTable = "cohort2"
#'   )
#' mockDisconnect(cdm = cdm)
#' }
#'
addCohortIntersectFlag <- function(x,
                                   targetCohortTable,
                                   targetCohortId = NULL,
                                   indexDate = "cohort_start_date",
                                   censorDate = NULL,
                                   targetStartDate = "cohort_start_date",
                                   targetEndDate = "cohort_end_date",
                                   window = list(c(0, Inf)),
                                   nameStyle = "{cohort_name}_{window_name}",
                                   name = NULL) {
  cdm <- omopgenerics::cdmReference(x)
  omopgenerics::assertCharacter(targetCohortTable)
  checkCdm(cdm, tables = targetCohortTable)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], {{targetCohortId}}, targetCohortTable)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)

  x <- x |>
    .addIntersect(
      tableName = targetCohortTable,
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = parameters$id_name,
      value = "flag",
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      nameStyle = nameStyle,
      censorDate = censorDate,
      name = name
    )

  return(x)
}

#' It creates columns to indicate number of occurrences of intersection with a
#' cohort
#'
#' @param x Table with individuals in the cdm.
#' @param targetCohortTable name of the cohort that we want to check for overlap.
#' @param targetCohortId vector of cohort definition ids to include.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x.
#' @param targetStartDate date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence).
#' @param targetEndDate date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence).
#' @param window window to consider events of.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters.
#' @param name Name of the new table, if NULL a temporary table is returned.
#'
#' @return table with added columns with overlap information.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 |>
#'   addCohortIntersectCount(
#'     targetCohortTable = "cohort2"
#'   )
#'
#' mockDisconnect(cdm = cdm)
#' }
#'
addCohortIntersectCount <- function(x,
                                    targetCohortTable,
                                    targetCohortId = NULL,
                                    indexDate = "cohort_start_date",
                                    censorDate = NULL,
                                    targetStartDate = "cohort_start_date",
                                    targetEndDate = "cohort_end_date",
                                    window = list(c(0, Inf)),
                                    nameStyle = "{cohort_name}_{window_name}",
                                    name = NULL) {
  cdm <- omopgenerics::cdmReference(x)
  omopgenerics::assertCharacter(targetCohortTable)
  checkCdm(cdm, tables = targetCohortTable)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], {{targetCohortId}}, targetCohortTable)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)

  x <- x |>
    .addIntersect(
      tableName = targetCohortTable,
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = parameters$id_name,
      value = "count",
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      nameStyle = nameStyle,
      censorDate = censorDate,
      name = name
    )

  return(x)
}

#' It creates columns to indicate the number of days between the current table
#' and a target cohort
#'
#' @param x Table with individuals in the cdm.
#' @param targetCohortTable Cohort table to.
#' @param targetCohortId Cohort IDs of interest from the other cohort table. If
#' NULL, all cohorts will be used with a days variable added for each
#' cohort of interest.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x.
#' @param targetDate Date of interest in the other cohort table. Either
#' cohort_start_date or cohort_end_date.
#' @param order date to use if there are multiple records for an
#' individual during the window of interest. Either first or last.
#' @param window Window of time to identify records relative to the indexDate.
#' Records outside of this time period will be ignored.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters.
#' @param name Name of the new table, if NULL a temporary table is returned.
#'
#' @return x along with additional columns for each cohort of interest.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 |>
#'   addCohortIntersectDays(targetCohortTable = "cohort2")
#'
#' mockDisconnect(cdm = cdm)
#' }
#'
addCohortIntersectDays <- function(x,
                                   targetCohortTable,
                                   targetCohortId = NULL,
                                   indexDate = "cohort_start_date",
                                   censorDate = NULL,
                                   targetDate = "cohort_start_date",
                                   order = "first",
                                   window = c(0, Inf),
                                   nameStyle = "{cohort_name}_{window_name}",
                                   name = NULL) {
  cdm <- omopgenerics::cdmReference(x)
  omopgenerics::assertCharacter(targetCohortTable)
  checkCdm(cdm, tables = targetCohortTable)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], {{targetCohortId}}, targetCohortTable)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)

  x <- x |>
    .addIntersect(
      tableName = targetCohortTable,
      indexDate = indexDate,
      value = "days",
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = parameters$id_name,
      window = window,
      targetStartDate = targetDate,
      targetEndDate = NULL,
      order = order,
      nameStyle = nameStyle,
      censorDate = censorDate,
      name = name
    )

  return(x)
}


#' Date of cohorts that are present in a certain window
#'
#' @param x Table with individuals in the cdm.
#' @param targetCohortTable Cohort table to.
#' @param targetCohortId Cohort IDs of interest from the other cohort table. If
#' NULL, all cohorts will be used with a time variable added for each
#' cohort of interest.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x.
#' @param targetDate Date of interest in the other cohort table. Either
#' cohort_start_date or cohort_end_date.
#' @param order date to use if there are multiple records for an
#' individual during the window of interest. Either first or last.
#' @param window Window of time to identify records relative to the indexDate.
#' Records outside of this time period will be ignored.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters.
#' @param name Name of the new table, if NULL a temporary table is returned.
#'
#' @return x along with additional columns for each cohort of interest.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 |>
#'   addCohortIntersectDate(targetCohortTable = "cohort2")
#'
#' mockDisconnect(cdm = cdm)
#' }
#'
addCohortIntersectDate <- function(x,
                                   targetCohortTable,
                                   targetCohortId = NULL,
                                   indexDate = "cohort_start_date",
                                   censorDate = NULL,
                                   targetDate = "cohort_start_date",
                                   order = "first",
                                   window = c(0, Inf),
                                   nameStyle = "{cohort_name}_{window_name}",
                                   name = NULL) {
  cdm <- omopgenerics::cdmReference(x)
  omopgenerics::assertCharacter(targetCohortTable)
  checkCdm(cdm, tables = targetCohortTable)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], {{targetCohortId}}, targetCohortTable)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)

  x <- x |>
    .addIntersect(
      tableName = targetCohortTable,
      indexDate = indexDate,
      value = "date",
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = parameters$id_name,
      window = window,
      targetStartDate = targetDate,
      targetEndDate = NULL,
      order = order,
      nameStyle = nameStyle,
      censorDate = censorDate,
      name = name
    )

  return(x)
}
