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

#' Summarise the characteristics of different individuals
#'
#' @param table Table with different records
#' @param group List of groups to be considered.
#' @param includeOverallGroup TRUE or FALSE. If TRUE, results for an overall
#' group will be reported when a list of groups has been specified.
#' @param strata List of the stratifications within each group to be considered.
#' @param includeOverallStrata TRUE or FALSE. If TRUE, results for an overall
#' strata will be reported when a list of strata has been specified.
#' @param variables List of the different groups of variables, by default they
#' are automatically classified.
#' @param functions List of functions to be applied to each one of the group of
#' variables.
#' @param minCellCount Minimum count of records to report results.
#'
#' @return Table that summarises the characteristics of the individual.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' library(dplyr)
#'
#' cdm <- mockPatientProfiles()
#' x <- cdm$cohort1 %>%
#'   addDemographics(cdm) %>%
#'   collect()
#' result <- summariseResult(x)
#' }
#'
summariseResult <- function(table,
                            group = list(),
                            includeOverallGroup = FALSE,
                            strata = list(),
                            includeOverallStrata = TRUE,
                            variables = list(
                              numericVariables = detectVariables(table, "numeric"),
                              dateVariables = detectVariables(table, "date"),
                              binaryVariables = detectVariables(table, "binary"),
                              categoricalVariables = detectVariables(table, "categorical")
                            ),
                            functions = list(
                              numericVariables = c("median", "q25", "q75"),
                              dateVariables = c("median", "q25", "q75"),
                              binaryVariables = c("count", "%"),
                              categoricalVariables = c("count", "%")
                            ),
                            minCellCount = 5) {

  # collect table
  table <- table %>% dplyr::collect()

  # initial checks
  checkTable(table)
  checkStrata(group, table)
  checkStrata(strata, table)
  checkVariablesFunctions(variables, functions, table)
  checkSuppressCellCount(minCellCount)

  # create the summary for overall
  result <- list()
  if(isTRUE(includeOverallGroup) || length(group) == 0) {
  result <- table %>%
    summaryValuesStrata(
      strata, variables, functions, includeOverall = includeOverallStrata
    ) %>%
    dplyr::mutate(group_name = "Overall",
                  group_level = "Overall") %>%
    dplyr::select(dplyr::all_of(
     c("group_name", "group_level",
      "strata_name", "strata_level", "variable",
      "variable_level", "variable_type",
      "estimate_type", "estimate")
    )) %>%
    dplyr::arrange(.data$strata_name, .data$strata_level)
  }

  # add results for each group
 for(i in seq_along(group)){
  workingGroup <- group[[i]]
  workingGroupName <- names(group)[i]
  table <- table %>%
    tidyr::unite("group_var",
                 c(dplyr::all_of(.env$workingGroup)),
                 remove = FALSE, sep = " and ")
  workingGroupLevels <- table %>%
    dplyr::select(dplyr::all_of("group_var")) %>%
    dplyr::distinct() %>%
    dplyr::pull()

    for(j in seq_along(workingGroupLevels)){
     workingResult <- table %>%
        dplyr::filter(
          .data[["group_var"]] == workingGroupLevels[[j]]) %>%
        summaryValuesStrata(
          strata, variables, functions, includeOverall = includeOverallStrata
        ) %>%
        dplyr::mutate(group_name = workingGroupName,
                      group_level = workingGroupLevels[[j]]) %>%
        dplyr::select(dplyr::all_of(
          c("group_name", "group_level",
            "strata_name", "strata_level", "variable",
            "variable_level", "variable_type",
            "estimate_type", "estimate")
        )) %>%
        dplyr::arrange(.data$strata_name, .data$strata_level)

     result <- dplyr::bind_rows(result, workingResult)

    }



 }





  # obscure counts
  result <- supressCounts(result, minCellCount)

  return(result)
}

#' @noRd
getNumericValues <- function(x, variablesNumeric) {
  functions <- variablesNumeric %>%
    dplyr::pull("estimate_type") %>%
    unique()
  result <- NULL
  for (k in seq_along(functions)) {
    variablesFunction <- variablesNumeric %>%
      dplyr::filter(.data$estimate_type == .env$functions[k]) %>%
      dplyr::pull("variable")
    result <- result %>%
      dplyr::union_all(
        x %>%
          dplyr::summarise(dplyr::across(
            .cols = dplyr::all_of(variablesFunction),
            .fns = getFunctions(functions[k]),
            .names = "{.col}"
          )) %>%
          tidyr::pivot_longer(
            dplyr::all_of(variablesFunction),
            names_to = "variable",
            values_to = "estimate",
            values_transform = list(estimate = as.character)
          ) %>%
          dplyr::mutate(
            estimate_type = .env$functions[k], variable_type = "numeric"
          ) %>%
          dplyr::select(
            "strata_level", "variable", "variable_type", "estimate_type",
            "estimate"
          )
      )
  }
  return(result)
}

#' @noRd
getDateValues <- function(x, variablesDate) {
  functions <- variablesDate %>%
    dplyr::pull("estimate_type") %>%
    unique()
  result <- NULL
  for (k in seq_along(functions)) {
    variablesFunction <- variablesDate %>%
      dplyr::filter(.data$estimate_type == .env$functions[k]) %>%
      dplyr::pull("variable")
    resultK <- x %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(variablesFunction), as.numeric)) %>%
      dplyr::summarise(dplyr::across(
        .cols = dplyr::all_of(variablesFunction),
        .fns = getFunctions(functions[k]),
        .names = "{.col}"
      ))
    if (availableFunctions("date") %>%
      dplyr::filter(.data$format_key == functions[k]) %>%
      dplyr::pull("result") == "date") {
      resultK <- resultK %>%
        dplyr::mutate(dplyr::across(
          dplyr::all_of(variablesFunction),
          ~ as.character(as.Date(round(.x), origin = "1970-01-01"))
        ))
    }
    resultK <- resultK %>%
      tidyr::pivot_longer(
        dplyr::all_of(variablesFunction),
        names_to = "variable",
        values_to = "estimate"
      ) %>%
      dplyr::mutate(
        estimate_type = .env$functions[k], variable_type = "date"
      )
    result <- dplyr::union_all(result, resultK)
  }
  result <- result %>%
    dplyr::select(
      "strata_level", "variable", "variable_type", "estimate_type",
      "estimate"
    )
  return(result)
}

#' @noRd
getBinaryValues <- function(x, variablesBinary) {
  result <- NULL
  variablesFunction <- variablesBinary %>%
    dplyr::filter(.data$estimate_type %in% c("count", "%")) %>%
    dplyr::pull("variable") %>%
    unique()
  if (length(variablesFunction) > 0) {
    result <- result %>%
      dplyr::union_all(
        x %>%
          dplyr::mutate(denominator = 1) %>%
          dplyr::summarise(dplyr::across(
            .cols = dplyr::all_of(c(variablesFunction, "denominator")),
            .fns = list("sum" = function(x) {
              sum(x)
            }),
            .names = "{.col}"
          )) %>%
          tidyr::pivot_longer(
            dplyr::all_of(variablesFunction),
            names_to = "variable",
            values_to = "count"
          ) %>%
          dplyr::mutate("%" = 100 * .data$count / .data$denominator) %>%
          dplyr::select(-"denominator") %>%
          tidyr::pivot_longer(c("count", "%"),
            names_to = "estimate_type",
            values_to = "estimate"
          ) %>%
          dplyr::inner_join(
            variablesBinary %>%
              dplyr::select("variable", "variable_type", "estimate_type"),
            by = c("variable", "estimate_type")
          ) %>%
          dplyr::select(
            "strata_level", "variable", "variable_type", "estimate_type",
            "estimate"
          )
      ) %>%
      dplyr::mutate(estimate = as.character(.data$estimate))
  }
  variablesBinary <- variablesBinary %>%
    dplyr::filter(!(.data$estimate_type %in% c("count", "%")))
  if (nrow(variablesBinary) > 0) {
    result <- result %>%
      dplyr::union_all(
        getNumericValues(
          x, variablesBinary
        )
      )
  }
  return(result)
}

#' @noRd
getCategoricalValues <- function(x, variablesCategorical) {
  variables <- variablesCategorical %>%
    dplyr::pull("variable") %>%
    unique()
  result <- NULL
  denominator <- x %>%
    dplyr::summarise(denominator = dplyr::n())
  for (v in variables) {
    xx <- x %>%
      dplyr::select("strata_level", "variable_level" = dplyr::all_of(v)) %>%
      tidyr::separate_rows("variable_level", sep = "&&", convert = TRUE)
    functions <- variablesCategorical %>%
      dplyr::filter(.data$variable == .env$v) %>%
      dplyr::pull("estimate_type") %>%
      unique()
    if (length(functions[functions != "distinct"]) > 0) {
      categories <- xx %>%
        dplyr::ungroup() %>%
        dplyr::select("variable_level") %>%
        dplyr::distinct()
      summaryX <- xx %>%
        dplyr::group_by(.data$variable_level, .add = TRUE) %>%
        dplyr::summarise(count = as.numeric(dplyr::n()), .groups = "drop") %>%
        dplyr::group_split(.data$strata_level) %>%
        lapply(function(x) {
          stra <- unique(x$strata_level)
          x <- x %>%
            dplyr::right_join(categories, by = "variable_level") %>%
            dplyr::mutate(strata_level = .env$stra)
          return(x)
        }) %>%
        dplyr::bind_rows() %>%
        dplyr::inner_join(denominator, by = "strata_level") %>%
        dplyr::mutate(count = dplyr::if_else(
          is.na(.data$count), 0, .data$count
        ))
    }
    if ("count" %in% functions | "%" %in% functions) {
      result <- result %>%
        dplyr::union_all(
          summaryX %>%
            dplyr::mutate("%" = 100 * .data$count / .data$denominator) %>%
            dplyr::select(-"denominator") %>%
            tidyr::pivot_longer(c("count", "%"),
              names_to = "estimate_type",
              values_to = "estimate"
            ) %>%
            dplyr::filter(.data$estimate_type %in% .env$functions) %>%
            dplyr::mutate(
              variable = .env$v, variable_type = "categorical"
            ) %>%
            dplyr::select(
              "strata_level", "variable",
              "variable_level", "variable_type",
              "estimate_type", "estimate"
            )
        )
    }
    if ("distinct" %in% functions) {
      result <- result %>%
        dplyr::union_all(
          xx %>%
            dplyr::summarise(
              estimate = dplyr::n_distinct(.data$variable_level), .groups = "drop"
            ) %>%
            dplyr::mutate(
              variable = .env$v, estimate_type = "distinct",
              variable_type = "categorical"
            )
        )
    }
    functions <- functions[!(functions %in% c("count", "%", "distinct"))]
    if (length(functions) > 0) {
      result <- result %>%
        dplyr::union_all(
          summaryX %>%
            dplyr::summarise(dplyr::across(
              .cols = "count",
              .fns = getFunctions(functions),
              .names = "{.fn}"
            )) %>%
            tidyr::pivot_longer(!"strata_level",
              names_to = "estimate_type",
              values_to = "estimate"
            ) %>%
            dplyr::mutate(
              variable = .env$v, variable_type = "categorical"
            )
        )
    }
  }
  return(result)
}

#' @noRd
summaryValues <- function(x, variables, functions) {
  # get which are the estimates that are needed
  requiredFunctions <- NULL
  for (nam in names(variables)) {
    requiredFunctions <- requiredFunctions %>%
      dplyr::union_all(
        tidyr::expand_grid(
          variable = variables[[nam]],
          estimate_type = functions[[nam]]
        )
      )
  }
  requiredFunctions <- requiredFunctions %>%
    dplyr::left_join(
      variableTypes(x) %>% dplyr::select(-"type_sum"),
      by = "variable"
    )

  # results
  result <- x %>%
    dplyr::summarise(estimate = as.character(dplyr::n()), .groups = "drop") %>%
    dplyr::mutate(
      variable = "number records", variable_type = as.character(NA),
      estimate_type = "count"
    )

  # count subjects
  result <- countSubjects(x) %>%
    dplyr::union_all(result)

  # numeric variables
  variablesNumeric <- requiredFunctions %>%
    dplyr::filter(.data$variable_type == "numeric")
  if (nrow(variablesNumeric) > 0) {
    result <- dplyr::union_all(
      result,
      getNumericValues(
        x, variablesNumeric
      ) %>%
        dplyr::mutate(estimate = as.character(.data$estimate)) %>%
        dplyr::arrange(.data$variable)
    )
  }

  # date variables
  variablesDate <- requiredFunctions %>%
    dplyr::filter(.data$variable_type == "date")
  if (nrow(variablesDate) > 0) {
    result <- dplyr::union_all(
      result,
      getDateValues(
        x, variablesDate
      ) %>%
        dplyr::arrange(.data$variable)
    )
  }

  # binary variables
  variablesBinary <- requiredFunctions %>%
    dplyr::filter(.data$variable_type == "binary")
  if (nrow(variablesBinary) > 0) {
    result <- dplyr::union_all(
      result,
      getBinaryValues(
        x, variablesBinary
      ) %>%
        dplyr::mutate(estimate = as.character(.data$estimate)) %>%
        dplyr::arrange(.data$variable)
    )
  }

  result <- result %>%
    dplyr::mutate(variable_level = as.character(NA))

  # categorical variables
  variablesCategorical <- requiredFunctions %>%
    dplyr::filter(.data$variable_type == "categorical")
  if (nrow(variablesCategorical) > 0) {
    result <- dplyr::union_all(
      result,
      getCategoricalValues(
        x, variablesCategorical
      ) %>%
        dplyr::mutate(estimate = as.character(.data$estimate)) %>%
        dplyr::arrange(.data$variable)
    )
  }

  return(result)
}

#' @noRd
countSubjects <- function(x) {
  i <- "person_id" %in% colnames(x)
  j <- "subject_id" %in% colnames(x)
  if (i) {
    if (j) {
      cli::cli_alert_warning(
        "person_id and subject_id present in table, `person_id` used as person identifier"
      )
    }
    personVariable <- "person_id"
  } else if (j) {
    personVariable <- "subject_id"
  }
  if (i | j) {
    result <- x %>%
      dplyr::summarise(
        estimate = as.character(dplyr::n_distinct(.data[[personVariable]])),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        variable = "number subjects", variable_type = as.character(NA),
        estimate_type = "count"
      )
    return(result)
  } else {
    return(NULL)
  }
}

#' @noRd
summaryValuesStrata <- function(x, strata,
                                variables,
                                functions,
                                includeOverall) {

  result <- list()
  if(isTRUE(includeOverall) || length(strata) == 0){
  result <- x %>%
    dplyr::mutate(strata_level = "Overall") %>%
    dplyr::group_by(.data$strata_level) %>%
    summaryValues(
      variables, functions
    ) %>%
    dplyr::mutate(strata_name = "Overall")
  }
  for (strat in names(strata)) {
    xx <- x %>%
      uniteStrata(strata[[strat]]) %>%
      tidyr::separate_rows("strata_level", sep = "&&", convert = FALSE) %>%
      dplyr::group_by(.data$strata_level)
    result <- result %>%
      dplyr::bind_rows(
        xx %>%
          summaryValues(
            variables, functions
          ) %>%
          dplyr::mutate(strata_name = .env$strat)
      )
  }
  result <- result %>%
    dplyr::relocate("strata_name")
  return(result)
}

#' @noRd
supressCounts <- function(result, minCellCount) {
  if (minCellCount > 1) {
    if ("number subjects" %in% result$variable) {
      personCount <- "number subjects"
    } else {
      personCount <- "number records"
    }
    toObscure <- result %>%
      dplyr::filter(.data$variable == .env$personCount) %>%
      dplyr::mutate(estimate = as.numeric(.data$estimate)) %>%
      dplyr::filter(.data$estimate > 0 & .data$estimate < .env$minCellCount) %>%
      dplyr::select("group_name","group_level", "strata_name", "strata_level")
    for (k in seq_along(toObscure)) {
      ik <- result$group_name == toObscure$group_name[k] &
        result$group_level == toObscure$group_level[k] &
        result$strata_name == toObscure$strata_name[k] &
        result$strata_level == toObscure$strata_level[k]
      is <- result$variable == personCount
      result$estimate[ik & is] <- paste0("<", minCellCount)
      result$estimate[ik & !is] <- as.character(NA)
    }
    estimate <- suppressWarnings(as.numeric(result$estimate))
    id <- unlist(lapply(strsplit(result$estimate_type, ": "), utils::tail, n = 1)) ==
      "count" & estimate < minCellCount & estimate > 0
    result <- result %>%
      dplyr::mutate(estimate = dplyr::if_else(
        .env$id, paste0("<", .env$minCellCount), .data$estimate
      ))
  }
  return(result)
}

uniteStrata <- function(x,
                        columns,
                        sepStrata = "&&",
                        sepStrataLevel = " and ") {
  combinations <- x %>%
    dplyr::select(dplyr::all_of(columns)) %>%
    dplyr::distinct()
  multiple <- combinations %>%
    dplyr::filter(dplyr::if_any(
      dplyr::all_of(columns), ~ grepl(.env$sepStrata, .)
    ))
  single <- combinations %>%
    dplyr::anti_join(multiple, by = columns) %>%
    tidyr::unite(
      "strata_level", dplyr::all_of(columns),
      sep = sepStrataLevel,
      remove = FALSE
    )
  multiple <- expandStrata(multiple, columns, sepStrata, sepStrataLevel)
  x <- x %>%
    dplyr::inner_join(dplyr::union_all(multiple, single), by = columns)
  return(x)
}

expandStrata <- function(x, columns, sepStrata, sepStrataLevel) {
  result <- NULL
  for (k in 1:nrow(x)) {
    result <- result %>%
      dplyr::union_all(dplyr::bind_cols(
        x[k, ],
        x[k, ] %>%
          tidyr::separate_longer_delim(dplyr::everything(), delim = sepStrata) %>%
          do.call(what = tidyr::expand_grid) %>%
          dplyr::distinct() %>%
          tidyr::unite(
            "strata_level", dplyr::all_of(columns),
            sep = sepStrataLevel
          )
      ))
  }
  return(result)
}
