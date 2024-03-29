# Copyright 2023 DARWIN EU (C)
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

#' Format a summarised_characteristics object into a visual table.
#'
#' @param result A summarised_characteristics object.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param format .
#' @param splitStrata .
#' @param format .
#' @param cdmName .
#' @param cohortName .
#' @param style .
#' @param minCellCount .
#' @param .options .
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 |>
#'   summariseCharacteristics() |>
#'   formatCharacteristics()
#' }
#'
#' @return A tibble with a tidy version of the summarised_characteristics
#' object.
#'
#' @export
#'
formatCharacteristics <- function(result,
                                  type = "gt",
                                  splitStrata = TRUE,
                                  format = c(
                                    "N (%)" = "<count> (<percentage>%)",
                                    "N" = "<count>",
                                    "<median> [<q25> - <q75>]",
                                    "<mean> (<sd>)",
                                    "range" = "<min> to <max>"
                                  ),
                                  cdmName = TRUE,
                                  cohortName = TRUE,
                                  style = "default",
                                  minCellCount = 5,
                                  .options = list()) {
  # check input
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "summarised_characteristics")
  checkmate::assertChoice(type, c("gt", "flextable"))
  checkmate::assertLogical(splitStrata, any.missing = FALSE, len = 1)
  checkmate::assertCharacter(format, any.missing = FALSE)
  checkmate::assertLogical(cdmName, any.missing = FALSE, len = 1)
  checkmate::assertLogical(cohortName, any.missing = FALSE, len = 1)
  checkmate::assertList(.options)
  checkmate::assertIntegerish(minCellCount, any.missing = FALSE, len = 1)

  # add default options
  .options <- defaultOptions(.options)

  result <- result |>
    suppress(minCellCount = minCellCount) |>
    visOmopResults::formatEstimateValue(
      decimals = .options$decimals,
      decimalMark = .options$decimalMark,
      bigMark = .options$bigMark
    ) |>
    visOmopResults::formatEstimateName(
      estimateNameFormat = format, keepNotFormatted = .options$keepNotFormatted
    ) |>
    dplyr::select(-c("result_type", "package_name", "package_version", "estimate_type")) |>
    visOmopResults::splitGroup(overall = FALSE) |>
    visOmopResults::splitAdditional(overall = FALSE)
  if (!"table" %in% colnames(result)) {
    result <- result |> dplyr::mutate("table" = NA_character_)
  }
  if (!"window" %in% colnames(result)) {
    result <- result |> dplyr::mutate("window" = NA_character_)
  }
  result <- result |>
    dplyr::mutate("variable_name" = dplyr::case_when(
      is.na(.data$table) & is.na(.data$window) ~ .data$variable_name,
      is.na(.data$table) & !is.na(.data$window) ~ paste(
        .data$variable_name, "in", .data$window
      ),
      !is.na(.data$table) & is.na(.data$window) ~ paste0(
        .data$variable_name, " [", .data$table, "]"
      ),
      !is.na(.data$table) & !is.na(.data$window) ~ paste0(
        .data$variable_name, " [", .data$table, "] in ", .data$window
      )
    )) |>
    dplyr::select(-c("table", "window"))

  colsStrata <- visOmopResults::strataColumns(result, overall = FALSE)
  if (length(colsStrata) > 0 & !splitStrata) {
    result <- result |>
      dplyr::mutate(
        "strata" = paste0(.data$strata_name, ": ", .data$strata_level)
      )
    colsStrata <- "strata"
  } else {
    result <- result |>
      visOmopResults::splitStrata(overall = FALSE)
  }

  headers <- character()
  if (cdmName) headers <- c(headers, "CDM name")
  if (cohortName) headers <- c(headers, "Cohort name")
  if (length(colsStrata) > 0) {
    headers <- c(headers, "strata")
    if (colsStrata != "strata") {
      headers <- c(headers, colsStrata)
    }
  }
  result <- result |>
    dplyr::rename(
      "CDM name" = "cdm_name", "Cohort name" = "cohort_name",
      "Variable" = "variable_name", "Level" = "variable_level",
      "Format" = "estimate_name"
    ) |>
    dplyr::mutate("Cohort name" = stringr::str_to_sentence(
      gsub("_", " ", .data[["Cohort name"]])
    )) |>
    visOmopResults::formatTable(
      header = headers, delim = "\n",
      includeHeaderName = .options$includeHeaderName,
      includeHeaderKey = TRUE
    )


  result <- switch(
    type,
    "gt" = visOmopResults::gtTable(
      x = result, delim = "\n", style = style, na = .options$na,
      title = .options$title, subtitle = .options$subtitle,
      caption = .options$caption, groupNameCol = .options$groupNameCol,
      groupNameAsColumn = .options$groupNameAsColumn,
      groupOrder = .options$groupOrder, colsToMergeRows = c("Variable", "Level")
    ),
    "flextable" = visOmopResults::fxTable(
      x = result, delim = "\n", style = style, na = .options$na,
      title = .options$title, subtitle = .options$subtitle,
      caption = .options$caption, groupNameCol = .options$groupNameCol,
      groupNameAsColumn = .options$groupNameAsColumn,
      groupOrder = .options$groupOrder, colsToMergeRows = c("Variable", "Level")
    )
  )

  return(result)
}

defaultOptions <- function(.options) {
  defaults <- list(
    "decimals" = c(integer = 0, numeric = 2, percentage = 1, proportion = 3),
    "decimalMark" = ".",
    "bigMark" = ",",
    "keepNotFormatted" = FALSE,
    "includeHeaderName" = TRUE,
    "na" = "-",
    "title" = NULL,
    "subtitle" = NULL,
    "caption" = NULL,
    "groupNameCol" = NULL,
    "groupNameAsColumn" = FALSE,
    "groupOrder" = NULL
  )
  for (nm in names(defaults)) {
    if (!nm %in% names(.options)) {
      .options[[nm]] <- defaults[[nm]]
    }
  }
  return(.options)
}
