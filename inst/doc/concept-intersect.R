## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(PatientProfiles)
library(CodelistGenerator)
library(CohortConstructor)
library(dplyr)
library(ggplot2)
library(omock)

cdm <- mockCdmFromDataset(datasetName = "GiBleed", source = "duckdb")

cdm$ankle_sprain <- conceptCohort(
  cdm = cdm,
  conceptSet = list("ankle_sprain" = 81151),
  name = "ankle_sprain"
)

cdm$ankle_sprain

## -----------------------------------------------------------------------------
acetaminophen_cs <- getDrugIngredientCodes(
  cdm = cdm,
  name = c("acetaminophen")
)

acetaminophen_cs

## -----------------------------------------------------------------------------
cdm$ankle_sprain |>
  addConceptIntersectFlag(
    conceptSet = acetaminophen_cs,
    indexDate = "cohort_start_date",
    window = c(0, 30)
  ) |>
  glimpse()

## -----------------------------------------------------------------------------
cdm$ankle_sprain |>
  addConceptIntersectCount(
    conceptSet = acetaminophen_cs,
    indexDate = "cohort_start_date",
    window = c(0, 30)
  ) |>
  glimpse()

## -----------------------------------------------------------------------------
cdm$ankle_sprain |>
  addConceptIntersectDate(
    conceptSet = acetaminophen_cs,
    indexDate = "cohort_start_date",
    window = c(0, 30),
    order = "first"
  ) |>
  glimpse()

## -----------------------------------------------------------------------------
cdm$ankle_sprain |>
  addConceptIntersectDays(
    conceptSet = acetaminophen_cs,
    indexDate = "cohort_start_date",
    window = c(0, 30),
    order = "first"
  ) |>
  glimpse()

## -----------------------------------------------------------------------------
cdm$ankle_sprain |>
  addConceptIntersectFlag(
    conceptSet = acetaminophen_cs,
    indexDate = "cohort_start_date",
    window = list(
      c(-Inf, -1),
      c(0, 0),
      c(1, Inf)
    )
  ) |>
  glimpse()

## -----------------------------------------------------------------------------
meds_cs <- getDrugIngredientCodes(
  cdm = cdm,
  name = c(
    "acetaminophen",
    "amoxicillin",
    "aspirin",
    "heparin",
    "morphine",
    "oxycodone",
    "warfarin"
  )
)

cdm$ankle_sprain |>
  addConceptIntersectFlag(
    conceptSet = meds_cs,
    indexDate = "cohort_start_date",
    window = list(
      c(-Inf, -1),
      c(0, 0)
    )
  ) |>
  glimpse()

## ----fig.width=7--------------------------------------------------------------
acetaminophen_cs <- getDrugIngredientCodes(
  cdm = cdm,
  name = c("acetaminophen")
)

cdm$acetaminophen <- conceptCohort(
  cdm = cdm,
  name = "acetaminophen",
  conceptSet = acetaminophen_cs
)

bind_rows(
  cdm$ankle_sprain |>
    addCohortIntersectCount(
      targetCohortTable = "acetaminophen",
      window = c(-Inf, Inf)
    ) |>
    group_by(`161_acetaminophen_minf_to_inf`) |>
    tally() |>
    collect() |>
    arrange(desc(`161_acetaminophen_minf_to_inf`)) |>
    mutate(type = "cohort"),
  cdm$ankle_sprain |>
    addConceptIntersectCount(
      conceptSet = acetaminophen_cs,
      window = c(-Inf, Inf)
    ) |>
    group_by(`161_acetaminophen_minf_to_inf`) |>
    tally() |>
    collect() |>
    arrange(desc(`161_acetaminophen_minf_to_inf`)) |>
    mutate(type = "concept_set")
) |>
  ggplot() +
  geom_col(aes(`161_acetaminophen_minf_to_inf`, n, fill = type),
    position = "dodge"
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

