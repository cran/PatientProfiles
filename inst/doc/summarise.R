## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE, eval = TRUE------------------------------------------------
PatientProfiles:::formats |>
  dplyr::rename(
    "Estimate name" = "estimate_name",
    "Description" = "estimate_description",
    "Estimate type" = "estimate_type"
  ) |>
  dplyr::group_by(.data$variable_type) |>
  gt::gt() |>
  gt::tab_style(
    style = gt::cell_fill(color = "#e1e1e1"),
    locations = gt::cells_row_groups()
  ) |>
  gt::tab_style(
    style = list(
      gt::cell_text(weight = "bold"),
      gt::cell_fill("#000"),
      gt::cell_text(color = "#fff")
    ),
    locations = gt::cells_column_labels()
  ) |>
  gt::tab_style(
    style = gt::cell_text(font = "consolas"),
    locations = gt::cells_body(columns = "Estimate name")
  )

## -----------------------------------------------------------------------------
library(PatientProfiles)
library(dplyr)
library(CohortConstructor)
library(CodelistGenerator)
library(omock)

cdm <- mockCdmFromDataset(datasetName = "GiBleed", source = "duckdb")

cdm$my_cohort <- conceptCohort(
  cdm = cdm,
  conceptSet = list("sinusitis" = c(4294548, 4283893, 40481087, 257012)),
  name = "my_cohort"
) |>
  requireIsFirstEntry()

cdm$drugs <- conceptCohort(
  cdm = cdm,
  conceptSet = getDrugIngredientCodes(cdm = cdm, name = c("morphine", "aspirin", "oxycodone")),
  name = "drugs"
)

x <- cdm$my_cohort |>
  # add demographics variables
  addDemographics() |>
  # add number of counts per ingredient before and after index date
  addCohortIntersectCount(
    targetCohortTable = "drugs",
    window = list("prior" = c(-Inf, -1), "future" = c(1, Inf)),
    nameStyle = "{window_name}_{cohort_name}"
  ) |>
  # add a flag regarding if they had a prior occurrence of pharyngitis
  addConceptIntersectFlag(
    conceptSet = list(pharyngitis = 4112343),
    window = c(-Inf, -1),
    nameStyle = "pharyngitis_before"
  ) |>
  # date fo the first visit for that individual
  addTableIntersectDate(
    tableName = "visit_occurrence",
    window = c(-Inf, Inf),
    nameStyle = "first_visit"
  ) |>
  # time till the next visit after sinusitis
  addTableIntersectDays(
    tableName = "visit_occurrence",
    window = c(1, Inf),
    nameStyle = "days_to_next_visit"
  )

x |>
  glimpse()

## -----------------------------------------------------------------------------
x |>
  group_by(sex) |>
  summarise(mean_age = mean(age), sd_age = sd(age))

## -----------------------------------------------------------------------------
x |>
  summariseResult(
    strata = "sex",
    variables = "age",
    estimates = c("mean", "sd"),
    counts = FALSE
  ) |>
  select(strata_name, strata_level, variable_name, estimate_value)

## -----------------------------------------------------------------------------
x |>
  summariseResult(
    strata = list("sex", "pharyngitis_before"),
    variables = "age",
    estimates = c("mean", "sd"),
    counts = FALSE
  ) |>
  select(strata_name, strata_level, variable_name, estimate_value)

## -----------------------------------------------------------------------------
x |>
  summariseResult(
    strata = list("sex", "pharyngitis_before", c("sex", "pharyngitis_before")),
    variables = "age",
    estimates = c("mean", "sd"),
    counts = FALSE
  ) |>
  select(strata_name, strata_level, variable_name, estimate_value) |>
  print(n = Inf)

## -----------------------------------------------------------------------------
x |>
  summariseResult(
    includeOverallStrata = FALSE,
    strata = list("sex", "pharyngitis_before"),
    variables = "age",
    estimates = c("mean", "sd"),
    counts = FALSE
  ) |>
  select(strata_name, strata_level, variable_name, estimate_value) |>
  print(n = Inf)

## -----------------------------------------------------------------------------
x |>
  addCohortName() |>
  summariseResult(
    group = "cohort_name",
    includeOverallGroup = FALSE,
    strata = list("sex", "pharyngitis_before"),
    includeOverallStrata = TRUE,
    variables = "age",
    estimates = c("mean", "sd"),
    counts = FALSE
  ) |>
  select(group_name, group_level, strata_name, strata_level, variable_name, estimate_value) |>
  print(n = Inf)

## -----------------------------------------------------------------------------
x |>
  summariseResult(
    variables = "age",
    estimates = c("mean", "sd"),
    counts = TRUE
  ) |>
  select(strata_name, strata_level, variable_name, estimate_value) |>
  print(n = Inf)

## -----------------------------------------------------------------------------
x |>
  summariseResult(
    strata = "pharyngitis_before",
    includeOverallStrata = FALSE,
    variables = list(c("age", "prior_observation"), "sex"),
    estimates = list(c("mean", "sd"), c("count", "percentage")),
    counts = FALSE
  ) |>
  select(strata_name, strata_level, variable_name, estimate_value) |>
  print(n = Inf)

## -----------------------------------------------------------------------------
drugs <- settings(cdm$drugs)$cohort_name
x |>
  addCohortName() |>
  summariseResult(
    group = "cohort_name",
    includeOverallGroup = FALSE,
    strata = list("pharyngitis_before"),
    includeOverallStrata = TRUE,
    variables = list(
      c(
        "age", "prior_observation", "future_observation", paste0("prior_", drugs),
        paste0("future_", drugs), "days_to_next_visit"
      ),
      c("sex", "pharyngitis_before"),
      c("first_visit", "cohort_start_date", "cohort_end_date")
    ),
    estimates = list(
      c("median", "q25", "q75"),
      c("count", "percentage"),
      c("median", "q25", "q75", "min", "max")
    ),
    counts = TRUE
  ) |>
  select(group_name, group_level, strata_name, strata_level, variable_name, estimate_value)

