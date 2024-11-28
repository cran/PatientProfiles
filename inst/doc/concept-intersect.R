## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = TRUE, echo = FALSE------------------------------------------------
library(CDMConnector)
if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
if (!dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))) dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))
if (!eunomia_is_available()) downloadEunomiaData()

## -----------------------------------------------------------------------------
library(CDMConnector)
library(CodelistGenerator)
library(PatientProfiles)
library(dplyr)
library(ggplot2)

con <- DBI::dbConnect(duckdb::duckdb(),
  dbdir = CDMConnector::eunomia_dir()
)
cdm <- CDMConnector::cdm_from_con(con,
  cdm_schem = "main",
  write_schema = "main"
)

cdm <- generateConceptCohortSet(
  cdm = cdm,
  name = "ankle_sprain",
  conceptSet = list("ankle_sprain" = 81151),
  end = "event_end_date",
  limit = "all",
  overwrite = TRUE
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
  dplyr::glimpse()

## -----------------------------------------------------------------------------
cdm$ankle_sprain |>
  addConceptIntersectCount(
    conceptSet = acetaminophen_cs,
    indexDate = "cohort_start_date",
    window = c(0, 30)
  ) |>
  dplyr::glimpse()

## -----------------------------------------------------------------------------
cdm$ankle_sprain |>
  addConceptIntersectDate(
    conceptSet = acetaminophen_cs,
    indexDate = "cohort_start_date",
    window = c(0, 30),
    order = "first"
  ) |>
  dplyr::glimpse()

## -----------------------------------------------------------------------------
cdm$ankle_sprain |>
  addConceptIntersectDays(
    conceptSet = acetaminophen_cs,
    indexDate = "cohort_start_date",
    window = c(0, 30),
    order = "first"
  ) |>
  dplyr::glimpse()

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
  dplyr::glimpse()

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
  dplyr::glimpse()

## ----fig.width=7--------------------------------------------------------------
acetaminophen_cs <- getDrugIngredientCodes(
  cdm = cdm,
  name = c("acetaminophen")
)

cdm <- generateConceptCohortSet(
  cdm = cdm,
  name = "acetaminophen",
  conceptSet = acetaminophen_cs,
  end = "event_end_date",
  limit = "all"
)

dplyr::bind_rows(
  cdm$ankle_sprain |>
    addCohortIntersectCount(
      targetCohortTable = "acetaminophen",
      window = c(-Inf, Inf)
    ) |>
    dplyr::group_by(`161_acetaminophen_minf_to_inf`) |>
    dplyr::tally() |>
    dplyr::collect() |>
    dplyr::arrange(desc(`161_acetaminophen_minf_to_inf`)) |>
    dplyr::mutate(type = "cohort"),
  cdm$ankle_sprain |>
    addConceptIntersectCount(
      conceptSet = acetaminophen_cs,
      window = c(-Inf, Inf)
    ) |>
    dplyr::group_by(`161_acetaminophen_minf_to_inf`) |>
    dplyr::tally() |>
    dplyr::collect() |>
    dplyr::arrange(desc(`161_acetaminophen_minf_to_inf`)) |>
    dplyr::mutate(type = "concept_set")
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

