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

#' Categorize a numeric variable
#'
#' @param x Table with individuals in the cdm
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param variable Target variable that we want to categorize.
#' @param categories List of lists of named categories with lower and upper
#' limit.
#' @param missingCategoryValue Value to assign to those individuals not in
#' any named category. If NULL or NA, missing will values will be
#' given.
#' @param overlap TRUE if the categories given overlap
#' @param tablePrefix The stem for the permanent tables that will be created. If
#' NULL, temporary tables will be used throughout.
#'
#' @return tibble with the categorical variable added.
#'
#' @export
#'
#' @examples
#' #'
#' \donttest{
#' library(DBI)
#' library(duckdb)
#' library(PatientProfiles)
#' cohort1 <- dplyr::tibble(
#'   cohort_definition_id = c("1", "1", "1"),
#'   subject_id = c("1", "2", "3"),
#'   cohort_start_date = c(
#'     as.Date("2010-03-03"), as.Date("2010-03-01"), as.Date("2010-02-01")
#'   ),
#'   cohort_end_date = c(
#'     as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2013-01-01")
#'   )
#' )
#'
#' person <- dplyr::tibble(
#'   person_id = c("1", "2", "3"),
#'   gender_concept_id = c("8507", "8507", "8507"),
#'   year_of_birth = c(1980, 1970, 2000),
#'   month_of_birth = c(03, 07, NA),
#'   day_of_birth = c(NA, 02, 01)
#' )
#'
#' cdm <- mockPatientProfiles(person = person, cohort1 = cohort1)
#'
#' result <- cdm$cohort1 %>%
#'   addAge(cdm) %>%
#'   addCategories(
#'     variable = "age",
#'     categories = list("age_group" = list(
#'       "0 to 39" = c(0, 39), "40 to 79" = c(40, 79), "80 to 150" = c(80, 150)
#'     ))
#'   )
#' }
addCategories <- function(x,
                          cdm,
                          variable,
                          categories,
                          missingCategoryValue = "None",
                          overlap = FALSE,
                          tablePrefix = NULL) {
  if (!isTRUE(inherits(x, "tbl_dbi"))) {
    cli::cli_abort("x is not a table")
  }

  checkmate::assertCharacter(variable, len = 1, any.missing = FALSE)
  checkmate::assertTRUE(variable %in% colnames(x))
  checkmate::assertNumeric(dplyr::pull(utils::head(x, 1), variable))
  checkmate::assertList(
    categories,
    types = "list", any.missing = FALSE, unique = TRUE, min.len = 1
  )
  checkmate::assertCharacter(tablePrefix, len = 1, null.ok = TRUE)


  for (i in seq_along(categories)) {
    if (!is.null(names(categories)) && variable == names(categories)[i]) {
      cli::cli_warn(paste0(
        "Categories name '",
        names(categories)[i],
        "' already existed, the original variable has been overwritten."
      ))
    }
  }

  if (length(unique(names(categories))) < length((names(categories)))) {
    cli::cli_abort(
      "Categories have repeated names, please rename the groups."
    )
  }

  if (is.null(names(categories))) {
    nam <- paste0("category_", seq_along(categories))
  } else {
    nam <- names(categories)
  }

  categoryTibble <- list()
  for (k in seq_along(categories)) {
    categoryTibble[[nam[k]]] <- checkCategory(categories[[k]],
                                              overlap = overlap)
  }

  for (k in seq_along(categories)) {
    categoryTibbleK <- categoryTibble[[k]]
    name <- names(categoryTibble)[k]

    x <- dplyr::mutate(x, variable := .data[[variable]])
    x <- dplyr::mutate(x, !!name := as.character(NA))
    if (!overlap) {
      for (i in 1:nrow(categoryTibbleK)) {
        lower <- categoryTibbleK$lower_bound[i]
        upper <- categoryTibbleK$upper_bound[i]
        category <- categoryTibbleK$category_label[i]
        x <- x %>%
          dplyr::mutate(!!name := dplyr::if_else(
            is.na(.data[[name]]) &
              .data$variable >= .env$lower &
              .data$variable <= .env$upper,
            .env$category,
            .data[[name]]
          ))
      }
    } else {
      for (i in 1:nrow(categoryTibbleK)) {
        lower <- categoryTibbleK$lower_bound[i]
        upper <- categoryTibbleK$upper_bound[i]
        category <- categoryTibbleK$category_label[i]
        x <- x %>%
          dplyr::mutate(!!name := dplyr::if_else(
            is.na(.data[[name]]) &
              .data$variable >= .env$lower &
              .data$variable <= .env$upper,
            .env$category,
            dplyr::if_else(
              !is.na(.data[[name]]) &
                .data$variable >= .env$lower &
                .data$variable <= .env$upper,
              paste0(.data[[name]], "&&", .env$category),
              .data[[name]]
            )
          ))
      }
    }

    x <- dplyr::select(x, -"variable")

    # add missing as category
    if (!is.null(missingCategoryValue) && !is.na(missingCategoryValue)) {
      x <- x %>%
        dplyr::mutate(!!name := dplyr::if_else(!is.na(.data[[name]]),
          .data[[name]],
          .env$missingCategoryValue
        ))
    }

    if (!is.null(tablePrefix)) {
      x <- CDMConnector::computeQuery(
        x,
        name = paste0(
          tablePrefix,
          "_categories_added"
        ),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )
    } else {
      x <- CDMConnector::computeQuery(x)
    }
  }

  return(x)
}
