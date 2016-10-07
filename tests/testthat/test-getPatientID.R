context("getPatientID")


test_that("getPatientID works", {
  expect_equal(FluidigmValidation::getPatientID("PAT-ID33-DNA-ID141-Curettage-E4_S122"),"ID33")
  expect_error(FluidigmValidation::getPatientID("PATID33-DNA-ID141-Curettage-E4_S122"))
})

test_that("getPatientID throws error with wrong inputs",{
  expect_error(FluidigmValidation::getPatientID("PAT-CX33-DNA-ID141-Curettage-E4_S122"))
  expect_error(FluidigmValidation::getPatientID(1))
})
