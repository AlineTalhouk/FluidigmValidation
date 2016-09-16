#' Title function to test getPatient ID
#'
#' @return
#' @export
#'
#' @examples
test.getPatientID<-function(){
  checkEquals(FluidigmValidation::getPatientID("PAT-ID33-DNA-ID141-Curettage-E4_S122"),"ID33")
  checkException(FluidigmValidation::getPatientID("PATID33-DNA-ID141-Curettage-E4_S122"))
  checkException(FluidigmValidation::getPatientID("PAT-CX33-DNA-ID141-Curettage-E4_S122"))
  checkException(FluidigmValidation::getPatientID(1))
}
