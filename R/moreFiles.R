#' Title function for creating a file for each patient
#'
#' @param allRepetitiveData : a data frame with only data with repetitive positions
#'
#' @return write an excel file for each patient
#' @export
#'
#' @examples
moreFiles<-function(allRepetitiveData){
  patientIDs<-NULL
  for(i in 1:nrow(allRepetitiveData)){
    patientIDs<-c(patientIDs,getPatientID(allRepetitiveData$Name[i]))
  }
  uniqueIDs<-sort(unique(patientIDs))
  allRepetitiveData$patientID<-patientIDs
  for(j in 1:length(uniqueIDs)){
    dir.create(uniqueIDs[j])
    write.xlsx(subset(allRepetitiveData[allRepetitiveData$patientID==uniqueIDs[j],],select=-patientID),
               file = paste("./",uniqueIDs[j],"/",uniqueIDs[j],".xlsx",sep=""),
               row.names = F,sheetName = uniqueIDs[j])
  }
}
