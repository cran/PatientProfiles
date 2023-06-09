test_that("addSex, check imput length and type", {
  cdm <- mockPatientProfiles(connectionDetails, seed = 100, patient_size = 10, earliest_observation_start_date = as.Date("2010-01-01"), latest_observation_start_date = as.Date("2022-01-01"))
  expect_error(addSex("cdm$cohort1", cdm))
  expect_error(addSex(cdm$cohort1, "cdm"))
  expect_error(addSex(cdm$drug_strength, cdm))
  expect_error(addSex(cdm$cohort1, cdm, name = 2))
  expect_error(addSex(cdm$cohort1, cdm, name = c("name1", "name2")))
  expect_error(addSex(cdm, cdm))
})

test_that("addSex, works in both cohort and condition tables", {
  cdm <- mockPatientProfiles(connectionDetails, seed = 8, patient_size = 10, earliest_observation_start_date = as.Date("2010-01-01"), latest_observation_start_date = as.Date("2022-01-01"))
  cdm$cohort1 <- cdm$cohort1 %>% addSex(cdm)
  cdm$condition_occurrence <- cdm$condition_occurrence %>% addSex(cdm)
  expect_true("sex" %in% colnames(cdm$cohort1))
  expect_true(all(cdm$cohort1$sex == c("Male", "Female", "Female", "Female")))
  expect_true("sex" %in% colnames(cdm$condition_occurrence))
  expect_true(all(cdm$condition_occurrence$sex == c("Female", "Male", "Female", "Male", "Female", "Female", "Male", "Female", "Male", "Female")))
})

test_that("addSex, desired result for all parameters", {
  cdm <- mockPatientProfiles(connectionDetails, seed = 27, patient_size = 10, earliest_observation_start_date = as.Date("2010-01-01"), latest_observation_start_date = as.Date("2022-01-01"))
  cdm$cohort2 <- cdm$cohort2 %>% addSex(cdm)
  expect_true("sex" %in% colnames(cdm$cohort2))
  expect_true(all(cdm$cohort2$sex == c("Male", "Male", "Male", "Male")))
  cdm$condition_occurrence <- cdm$condition_occurrence %>% addSex(cdm, sexName = "gender")
  expect_true("gender" %in% colnames(cdm$condition_occurrence))
  expect_false("sex" %in% colnames(cdm$condition_occurrence))
  expect_true(all(cdm$condition_occurrence$gender == c("Male", "Male", "Female", "Female", "Male", "Male", "Male", "Male", "Female", "Female")))
})

test_that("different names", {
  cdm <- mockPatientProfiles(connectionDetails, seed = 27, patient_size = 10, earliest_observation_start_date = as.Date("2010-01-01"), latest_observation_start_date = as.Date("2022-01-01"))
  cdm$cohort2 <- cdm$cohort2 %>% addSex(cdm, sexName = "gender")
  expect_true("gender" %in% colnames(cdm$cohort2))
})
