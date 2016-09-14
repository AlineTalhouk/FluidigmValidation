#' Title function to obtain patient ID from file name
#'
#' @param fileName : name of file
#'
#' @return The patient ID name
#' @export
#'
#' @examples "PAT-ID33-DNA-ID141-Curettage-E4_S122" returns "ID33"
getPatientID<-function(fileName){
  if(!is.character(fileName)){
    stop(paste("Error in getPatientID, file name", fileName, "is not character"))
  }
  patientID<-strsplit(fileName,split="[-]")[[1]][2]
  return(patientID)
}
