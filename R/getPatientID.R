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
  stopifnot(strsplit(fileName,split="[-]")[[1]][1]=="PAT")
  patientID<-strsplit(fileName,split="[-]")[[1]][2]
  stopifnot(substr(patientID,1,2)=="ID")
  return(patientID)
}
