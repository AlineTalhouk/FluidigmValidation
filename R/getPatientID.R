#' Title function to obtain patient ID from file name
#'
#' @param fileName : name of file
#'
#' @return The patient ID name
#' @export
#'
#' @examples "PAT-ID33-DNA-ID141-Curettage-E4_S122" returns "ID33"
getPatientID<-function(fileName){
  return(strsplit(fileName,split="[-]")[[1]][2])
}
