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

#' Compute demographic characteristics at a certain date
#'
#' @param x Table with individuals in the cdm
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param indexDate Variable in x that contains the date to compute the
#' demographics characteristics.
#' @param age TRUE or FALSE. If TRUE, age will be calculated relative to
#' indexDate
#' @param ageDefaultMonth Month of the year assigned to individuals with missing
#' month of birth.
#' @param ageName Age variable name
#' @param ageDefaultDay day of the month assigned to individuals
#' with missing day of birth.
#' @param ageImposeMonth TRUE or FALSE. Whether the month of the date of birth
#' will be considered as missing for all the individuals.
#' @param ageImposeDay TRUE or FALSE. Whether the day of the date of birth
#' will be considered as missing for all the individuals.
#' @param ageGroup if not NULL, a list of ageGroup vectors.
#' @param missingAgeGroupValue Value to include if missing age.
#' @param sex TRUE or FALSE. If TRUE, sex will be identified
#' @param sexName Sex variable name
#' @param missingSexValue Value to include if missing sex.
#' @param priorObservation TRUE or FALSE. If TRUE, days of between the start
#' of the current observation period and the indexDate will be calculated
#' @param priorObservationName Prior observation variable name
#' @param futureObservation TRUE or FALSE. If TRUE, days between the
#' indexDate and the end of the current observation period will be
#' calculated
#' @param futureObservationName Future observation variable name
#'
#' @return cohort table with the added demographic information columns
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addDemographics()
#' }
#'
addDemographics <- function(x,
                            cdm = lifecycle::deprecated(),
                            indexDate = "cohort_start_date",
                            age = TRUE,
                            ageName = "age",
                            ageDefaultMonth = 1,
                            ageDefaultDay = 1,
                            ageImposeMonth = FALSE,
                            ageImposeDay = FALSE,
                            ageGroup = NULL,
                            missingAgeGroupValue = "None",
                            sex = TRUE,
                            sexName = "sex",
                            missingSexValue = "None",
                            priorObservation = TRUE,
                            priorObservationName = "prior_observation",
                            futureObservation = TRUE,
                            futureObservationName = "future_observation") {
  ## change ageDefaultMonth, ageDefaultDay to integer
  if (lifecycle::is_present(cdm)) {
    lifecycle::deprecate_warn("0.6.0", "addDemographics(cdm)")
  }
  cdm <- omopgenerics::cdmReference(x)

  if (typeof(ageDefaultMonth) == "character") {
    ageDefaultMonth <- as.integer(ageDefaultMonth)
  }

  if (typeof(ageDefaultDay) == "character") {
    ageDefaultDay <- as.integer(ageDefaultDay)
  }

  ## check for standard types of user error
  personVariable <- checkX(x)
  checkCdm(cdm, c("person", "observation_period"))
  checkmate::assertLogical(age, any.missing = FALSE, len = 1)
  checkmate::assertIntegerish(
    ageDefaultMonth,
    lower = 1, upper = 31, any.missing = FALSE, len = 1,
    null.ok = !age
  )
  checkmate::assertIntegerish(
    ageDefaultDay,
    lower = 1, upper = 31, any.missing = FALSE, len = 1,
    null.ok = !age
  )
  checkmate::assertLogical(ageImposeMonth, any.missing = FALSE, len = 1)
  checkmate::assertLogical(ageImposeDay, any.missing = FALSE, len = 1)
  ageGroup <- checkAgeGroup(ageGroup)
  checkmate::assertLogical(sex, any.missing = FALSE, len = 1)
  checkmate::assertLogical(priorObservation, any.missing = FALSE, len = 1)
  checkmate::assertLogical(futureObservation, any.missing = FALSE, len = 1)
  checkVariableInX(indexDate, x, !(age | priorObservation | futureObservation))
  if (!(age | sex | priorObservation | futureObservation)) {
    cli::cli_abort("age, sex, priorObservation, futureObservation can not be FALSE")
  }
  checkmate::assertCharacter(missingAgeGroupValue, len = 1, any.missing = FALSE)
  checkmate::assertCharacter(missingSexValue, len = 1, any.missing = FALSE)

  # check variable names
  name <- character()
  if (age) {
    ageName <- checkSnakeCase(ageName)
    name <- c(name, ageName)
  }
  if (sex) {
    sexName <- checkSnakeCase(sexName)
    name <- c(name, sexName)
  }
  if (priorObservation) {
    priorObservationName <- checkSnakeCase(priorObservationName)
    name <- c(name, priorObservationName)
  }
  if (futureObservation) {
    futureObservationName <- checkSnakeCase(futureObservationName)
    name <- c(name, futureObservationName)
  }

  checkNewName(name = name, x = x)

  if (age == TRUE || priorObservation == TRUE || futureObservation == TRUE) {
    checkmate::assert_true(
      inherits(
        x %>%
          utils::head(1) %>%
          dplyr::pull(indexDate),
        c("Date", "POSIXt")
      )
    )
  }

  # Start code
  startTibble <- x
  startNames <- colnames(x)

  personDetails <- cdm[["person"]] %>%
    dplyr::select(
      "person_id",
      "gender_concept_id",
      "year_of_birth",
      "month_of_birth",
      "day_of_birth"
    ) %>%
    dplyr::rename(!!personVariable := "person_id")

  if (priorObservation == TRUE || futureObservation == TRUE) {
    # most recent observation period (in case there are multiple)
    obsPeriodDetails <- x %>%
      dplyr::select(dplyr::all_of(c(personVariable, indexDate))) %>%
      dplyr::distinct() %>%
      dplyr::inner_join(
        cdm[["observation_period"]] %>%
          dplyr::rename(!!personVariable := "person_id") %>%
          dplyr::select(
            dplyr::all_of(personVariable),
            "observation_period_start_date",
            "observation_period_end_date"
          ),
        by = personVariable
      ) %>%
      dplyr::filter(.data$observation_period_start_date <=
        .data[[indexDate]] &
        .data$observation_period_end_date >=
          .data[[indexDate]])
  }

  # update dates
  if (age) {
    personDetails <- personDetails %>%
      dplyr::filter(!is.na(.data$year_of_birth)) %>%
      addDateOfBirth(
        name = "date_of_birth",
        missingDay = ageDefaultDay,
        missingMonth = ageDefaultMonth,
        imposeDay = ageImposeDay,
        imposeMonth = ageImposeMonth
      )
  }

  # join if not the person table
  if (any(!c("person_id", "gender_concept_id") %in% colnames(x))) {

    addCols <- colnames(personDetails)[
      which(colnames(personDetails) != personVariable)]

    if(any(addCols %in%
       colnames(x))
    ){
      checkNewName(name = addCols, x = x)
      x <- x %>%
        dplyr::select(!dplyr::any_of(addCols))
    }

    x <- x %>%
      dplyr::left_join(
        personDetails %>%
          dplyr::select(dplyr::any_of(c(
            personVariable,
            addCols
          ))),
        by = personVariable
      )
  }

  if (priorObservation == TRUE || futureObservation == TRUE) {

    addCols <- colnames(obsPeriodDetails)[
      which(!colnames(obsPeriodDetails) %in% c(personVariable, indexDate))]

    if(any(addCols %in%
           colnames(x))
    ){
      checkNewName(name = addCols, x = x)
      x <- x %>%
        dplyr::select(!dplyr::any_of(addCols))
    }

    x <- x %>%
      dplyr::left_join(obsPeriodDetails,
        by = c(personVariable, indexDate)
      )
  }

  if (age == TRUE) {
    aQ <- ageQuery(indexDate, name = ageName)
  } else {
    aQ <- NULL
  }

  if (sex == TRUE) {
    sQ <- sexQuery(name = sexName, missingValue = missingSexValue)
  } else {
    sQ <- NULL
  }

  if (priorObservation == TRUE) {
    pHQ <- priorObservationQuery(indexDate, name = priorObservationName)
  } else {
    pHQ <- NULL
  }

  if (futureObservation == TRUE) {
    fOQ <- futureObservationQuery(indexDate, name = futureObservationName)
  } else {
    fOQ <- NULL
  }

  x <- x %>%
    dplyr::mutate(
      !!!aQ,
      !!!sQ,
      !!!pHQ,
      !!!fOQ
    )

  x <- x %>%
    dplyr::select(
      dplyr::all_of(startNames),
      dplyr::any_of(c(
        ageName, sexName,
        priorObservationName,
        futureObservationName
      ))
    )

  if (sex == TRUE) {
    x <- x %>%
      dplyr::mutate(!!sexName := dplyr::if_else(!is.na(.data[[sexName]]),
        .data[[sexName]],
        "None"
      ))
  }

  x <- x %>% dplyr::compute()

  if (!is.null(ageGroup)) {
    x <- addCategories(
      x = x,
      variable = ageName,
      categories = ageGroup,
      missingCategoryValue = missingAgeGroupValue
    )
  }

  return(x)
}



ageQuery <- function(indexDate, name) {
  return(glue::glue('floor(dbplyr::sql(
    CDMConnector::datediff(
      start = "date_of_birth",
      end = "{indexDate}",
      interval = "year"
    )
  ))') %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue(name)))
}

sexQuery <- function(name, missingValue) {
  return(glue::glue('dplyr::case_when(
      .data$gender_concept_id == 8507 ~ "Male",
      .data$gender_concept_id == 8532 ~ "Female",
      TRUE ~ "{missingValue}")') %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue(name)))
}

priorObservationQuery <- function(indexDate, name) {
  return(glue::glue('CDMConnector::datediff("observation_period_start_date",
                      "{indexDate}")') %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue(name)))
}

futureObservationQuery <- function(indexDate, name) {
  return(glue::glue('CDMConnector::datediff("{indexDate}",
                          "observation_period_end_date")') %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue(name)))
}

#' Compute the age of the individuals at a certain date
#'
#' @param x Table with individuals in the cdm.
#' @param cdm A cdm_reference object.
#' @param indexDate Variable in x that contains the date to compute the age.
#' @param ageName Name of the new column that contains age.
#' @param ageGroup List of age groups to be added.
#' @param ageDefaultMonth Month of the year assigned to individuals with missing
#' month of birth. By default: 1.
#' @param ageDefaultDay day of the month assigned to individuals with missing
#' day of birth. By default: 1.
#' @param ageImposeMonth Whether the month of the date of birth will be
#' considered as missing for all the individuals.
#' @param ageImposeDay Whether the day of the date of birth will be considered
#' as missing for all the individuals.
#' @param missingAgeGroupValue Value to include if missing age.
#'
#' @return tibble with the age column added
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 |>
#'   addAge()
#' }
addAge <- function(x,
                   cdm = lifecycle::deprecated(),
                   indexDate = "cohort_start_date",
                   ageName = "age",
                   ageGroup = NULL,
                   ageDefaultMonth = 1,
                   ageDefaultDay = 1,
                   ageImposeMonth = FALSE,
                   ageImposeDay = FALSE,
                   missingAgeGroupValue = "None") {
  if (lifecycle::is_present(cdm)) {
    lifecycle::deprecate_warn("0.6.0", "addAge(cdm)")
  }
  x <- x %>%
    addDemographics(
      indexDate = indexDate,
      age = TRUE,
      ageName = ageName,
      ageGroup = ageGroup,
      ageDefaultDay = ageDefaultDay,
      ageDefaultMonth = ageDefaultMonth,
      ageImposeDay = ageImposeDay,
      ageImposeMonth = ageImposeMonth,
      missingAgeGroupValue = missingAgeGroupValue,
      sex = FALSE,
      priorObservation = FALSE,
      futureObservation = FALSE,
      sexName = NULL,
      priorObservationName = NULL,
      futureObservationName = NULL
    )

  return(x)
}

#' Compute the number of days till the end of the observation period at a
#' certain date
#'
#' @param x Table with individuals in the cdm.
#' @param cdm A cdm_reference object.
#' @param indexDate Variable in x that contains the date to compute the future
#' observation.
#' @param futureObservationName name of the new column to be added
#'
#' @return cohort table with added column containing future observation of the
#' individuals
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addFutureObservation()
#' }
addFutureObservation <- function(x,
                                 cdm = lifecycle::deprecated(),
                                 indexDate = "cohort_start_date",
                                 futureObservationName = "future_observation") {
  if (lifecycle::is_present(cdm)) {
    lifecycle::deprecate_warn("0.6.0", "addFutureObservation(cdm)")
  }
  x <- x %>%
    addDemographics(
      indexDate = indexDate,
      age = FALSE,
      ageGroup = NULL,
      ageDefaultDay = NULL,
      ageDefaultMonth = NULL,
      ageImposeDay = FALSE,
      ageImposeMonth = FALSE,
      sex = FALSE,
      priorObservation = FALSE,
      futureObservation = TRUE,
      futureObservationName = futureObservationName,
      ageName = NULL,
      sexName = NULL,
      priorObservationName = NULL
    )

  return(x)
}

#' Compute the number of days of prior observation in the current observation period
#' at a certain date
#'
#' @param x Table with individuals in the cdm
#' @param cdm A cdm_reference object.
#' @param indexDate Variable in x that contains the date to compute the prior
#' observation.
#' @param priorObservationName name of the new column to be added
#'
#' @return cohort table with added column containing prior observation of the
#' individuals
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addPriorObservation()
#' }
addPriorObservation <- function(x,
                                cdm = lifecycle::deprecated(),
                                indexDate = "cohort_start_date",
                                priorObservationName = "prior_observation") {
  if (lifecycle::is_present(cdm)) {
    lifecycle::deprecate_warn("0.6.0", "addPriorObservation(cdm)")
  }
  x <- x %>%
    addDemographics(
      indexDate = indexDate,
      age = FALSE,
      ageGroup = NULL,
      ageDefaultDay = NULL,
      ageDefaultMonth = NULL,
      ageImposeDay = FALSE,
      ageImposeMonth = FALSE,
      sex = FALSE,
      priorObservation = TRUE,
      priorObservationName = priorObservationName,
      futureObservation = FALSE,
      ageName = NULL,
      sexName = NULL,
      futureObservationName = NULL
    )

  return(x)
}

#' Indicate if a certain record is within the observation period
#'
#' @param x Table with individuals in the cdm.
#' @param cdm A cdm_reference object.
#' @param indexDate Variable in x that contains the date to compute the
#' observation flag.
#' @param name name of the column to hold the result of the query:
#' 1 if the individual is in observation, 0 if not
#'
#' @return cohort table with the added binary column assessing inObservation
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addInObservation()
#' }
#'
addInObservation <- function(x,
                             cdm = lifecycle::deprecated(),
                             indexDate = "cohort_start_date",
                             name = "in_observation") {
  if (lifecycle::is_present(cdm)) {
    lifecycle::deprecate_warn("0.6.0", "inObservation(cdm)")
  }
  ## check for standard types of user error
  cdm <- omopgenerics::cdmReference(x)
  personVariable <- checkX(x)
  checkCdm(cdm, c("observation_period"))
  checkVariableInX(indexDate, x)
  checkmate::assertCharacter(name, any.missing = FALSE, len = 1)
  name <- checkNewName(name, x)

  # Start code
  name <- rlang::enquo(name)

  x <- x %>%
    addDemographics(
      indexDate = indexDate,
      age = FALSE,
      sex = FALSE,
      priorObservation = TRUE,
      futureObservation = TRUE
    ) %>%
    dplyr::mutate(
      !!name := as.numeric(dplyr::if_else(
        is.na(.data$prior_observation) | is.na(.data$future_observation) | .data$prior_observation < 0 | .data$future_observation < 0, 0, 1
      ))
    ) %>%
    dplyr::select(
      -"prior_observation", -"future_observation"
    )

  x <- x %>% dplyr::compute()

  return(x)
}

#' Compute the sex of the individuals
#'
#' @param x Table with individuals in the cdm
#' @param cdm A cdm_reference object.
#' @param sexName name of the new column to be added.
#' @param missingSexValue Value to include if missing sex.
#'
#' @return table x with the added column with sex information
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addSex()
#' }
#'
addSex <- function(x,
                   cdm = lifecycle::deprecated(),
                   sexName = "sex",
                   missingSexValue = "None") {
  if (lifecycle::is_present(cdm)) {
    lifecycle::deprecate_warn("0.6.0", "addSex(cdm)")
  }
  x <- x %>%
    addDemographics(
      indexDate = NULL,
      age = FALSE,
      ageGroup = NULL,
      ageDefaultDay = NULL,
      ageDefaultMonth = NULL,
      ageImposeDay = FALSE,
      ageImposeMonth = FALSE,
      sex = TRUE,
      sexName = sexName,
      missingSexValue = missingSexValue,
      priorObservation = FALSE,
      futureObservation = FALSE,
      ageName = NULL,
      priorObservationName = NULL,
      futureObservationName = NULL
    )

  return(x)
}
