test_that("test summariseCharacteristics", {
  person <- dplyr::tibble(
    person_id = c(1, 2, 3), gender_concept_id = c(8507, 8532, 8532),
    year_of_birth = c(1985, 2000, 1962), month_of_birth = c(10, 5, 9),
    day_of_birth = c(30, 10, 24),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  dus_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25"
    )),
    cohort_end_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25"
    ))
  )
  comorbidities <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 2, 1),
    subject_id = c(1, 1, 3, 3),
    cohort_start_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    ))
  )
  medication <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    ))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 2, 3),
    observation_period_start_date = as.Date(c(
      "1985-01-01", "1989-04-29", "1974-12-03"
    )),
    observation_period_end_date = as.Date(c(
      "2011-03-04", "2022-03-14", "2023-07-10"
    )),
    period_type_concept_id = 0
  )

  cdm <- mockPatientProfiles(
    connectionDetails,
    dus_cohort = dus_cohort, person = person,
    comorbidities = comorbidities, medication = medication,
    observation_period = observation_period, cohort1 = emptyCohort,
    cohort2 = emptyCohort
  )

  cdm$dus_cohort <- omopgenerics::newCohortTable(
    table = cdm$dus_cohort, cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1, 2), cohort_name = c("exposed", "unexposed")
    ))
  cdm$comorbidities <- omopgenerics::newCohortTable(
    table = cdm$comorbidities, cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1, 2), cohort_name = c("covid", "headache")
    ))
  cdm$medication <- omopgenerics::newCohortTable(
    table = cdm$medication,
    cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1, 2, 3),
      cohort_name = c("acetaminophen", "ibuprophen", "naloxone")
    ),
    cohortAttritionRef = NULL
  )

  expect_no_error(result <- summariseCharacteristics(
    cdm$dus_cohort,
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = "medication", value = "flag", window = c(-365, 0)
      ), "Comorbidities" = list(
        targetCohortTable = "comorbidities", value = "flag", window = c(-Inf, 0)
      )
    )
  ) |>
    suppress(minCellCount = 1))
  expect_true(inherits(result, "summarised_result"))
  expect_identical(
    result %>%
      dplyr::filter(group_level == "exposed") %>%
      dplyr::filter(variable_name == "Covid") %>%
      dplyr::filter(estimate_name == "count") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    2
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "exposed") %>%
      dplyr::filter(variable_name == "Headache") %>%
      dplyr::filter(estimate_name == "count") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    1
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "exposed") %>%
      dplyr::filter(variable_name == "Acetaminophen") %>%
      dplyr::filter(estimate_name == "count") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    2
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "exposed") %>%
      dplyr::filter(variable_name == "Ibuprophen") %>%
      dplyr::filter(estimate_name == "count") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    0
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "exposed") %>%
      dplyr::filter(variable_name == "Naloxone") %>%
      dplyr::filter(estimate_name == "count") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    0
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "unexposed") %>%
      dplyr::filter(variable_name == "Covid") %>%
      dplyr::filter(estimate_name == "count") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    0
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "unexposed") %>%
      dplyr::filter(variable_name == "Headache") %>%
      dplyr::filter(estimate_name == "count") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    1
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "unexposed") %>%
      dplyr::filter(variable_name == "Acetaminophen") %>%
      dplyr::filter(estimate_name == "count") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    0
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "unexposed") %>%
      dplyr::filter(variable_name == "Ibuprophen") %>%
      dplyr::filter(estimate_name == "count") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    0
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "unexposed") %>%
      dplyr::filter(variable_name == "Naloxone") %>%
      dplyr::filter(estimate_name == "count") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    0
  )

  expect_no_error(result <- summariseCharacteristics(
    cdm$dus_cohort,
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = "medication", value = "flag", window = list(
          "short" = c(-30, 0), "long" = c(-365, 0)
        )
      ), "Comorbidities" = list(
        targetCohortTable = "comorbidities", value = "flag", window = c(-Inf, 0)
      )
    )
  )|>
    suppress(minCellCount = 1))
  expect_true(inherits(result, "summarised_result"))
  expect_true(
    result %>%
      visOmopResults::splitAdditional() |>
      dplyr::filter(window == "short") %>%
      dplyr::tally() %>%
      dplyr::pull() ==
      omopgenerics::settings(cdm$medication) |> nrow() * 4 # 2 group_level 4 estimate type
  )
  expect_true(
    result %>%
      visOmopResults::splitAdditional() |>
      dplyr::filter(window == "long") %>%
      dplyr::tally() %>%
      dplyr::pull() ==
      omopgenerics::settings(cdm$medication) |> nrow() * 4 # 2 group_level 4 estimate type
  )
  expect_true(
    result %>%
      visOmopResults::splitAdditional() |>
      dplyr::filter(table == "medication") %>%
      dplyr::tally() %>%
      dplyr::pull() ==
      omopgenerics::settings(cdm$medication) |> nrow() * 8 # 2 group_level 4 estimate type 2 window
  )
  expect_true(
    result %>%
      visOmopResults::splitAdditional() |>
      dplyr::filter(table == "comorbidities") %>%
      dplyr::tally() %>%
      dplyr::pull() ==
      omopgenerics::settings(cdm$comorbidities) |> nrow() * 4 # 2 group_level 4 estimate type
  )

  result_notables <- summariseCharacteristics(
    cdm$dus_cohort,
    cohortIntersect = list(), tableIntersect = list()
  )|>
    suppress(minCellCount = 1)
  expect_true(inherits(result, "summarised_result"))

  # demographics
  expect_no_error(result <- summariseCharacteristics(
    cdm$dus_cohort,
    demographics = TRUE,
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = "medication", value = "flag", window = c(-365, 0)
      )
    )
  ))
  expect_true(all(
    c("Cohort start date", "Cohort end date", "Age", "Sex", "Prior observation",
      "Future observation") %in% result$variable_name
  ))
  expect_no_error(result <- summariseCharacteristics(
    cdm$dus_cohort,
    demographics = TRUE
  ))
  expect_true(all(
    c("Cohort start date", "Cohort end date", "Age", "Sex", "Prior observation",
      "Future observation") %in% result$variable_name
  ))
  expect_no_error(result <- summariseCharacteristics(
    cdm$dus_cohort,
    demographics = FALSE,
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = "medication", value = "flag", window = c(-365, 0)
      )
    )
  ))
  expect_false(any(
    c("Cohort start date", "Cohort end date", "Age", "Sex", "Prior observation",
      "Future observation") %in% result$variable_name
  ))
  expect_error(summariseCharacteristics(
    cdm$dus_cohort,
    demographics = FALSE
  ))

})

test_that("test empty cohort", {
  cdm <- mockPatientProfiles(connectionDetails = connectionDetails)

  expect_no_error(
    cdm$cohort1 %>% dplyr::filter(cohort_definition_id == 0) %>%
      summariseCharacteristics(cohortIntersect = list(
        "Medications" = list(
          targetCohortTable = "cohort2", value = "flag", window = c(-365, 0)
        ), "Comorbidities" = list(
          targetCohortTable = "cohort2", value = "flag", window = c(-Inf, 0)
        )
      ))
  )
  expect_no_error(
    cdm$cohort1 %>%
      summariseCharacteristics(cohortIntersect = list(
        "Medications" = list(
          targetCohortTable = "cohort2", value = "flag", window = c(-365, 0), targetCohortId = 1
        ), "Comorbidities" = list(
          targetCohortTable = "cohort2", value = "flag", window = c(-Inf, 0)
        )
      ))
  )
  expect_no_error(
    x1 <- cdm$cohort1 %>%
      summariseCharacteristics(tableIntersect = list("Visits" = list(
        tableName = "visit_occurrence", value = "flag", window = c(-365, 0)
      )))
  )

  expect_no_error(
    x3 <- cdm$cohort1 %>%
      summariseCharacteristics(tableIntersect = list("Visits" = list(
        tableName = "visit_occurrence", value = "visit_concept_id",
        window = c(-Inf, Inf)
      )))
  )
})
