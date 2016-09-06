#' Title function to label tumor type based on file name
#'
#' @param data : fluidigm data
#'
#' @return data whose rows are labeled "T1" if coming from a file with "Curettage", "T2" if coming
#' from a file containing "OP" and "N" if from a file containing "NORMAL"
#' @export
#'
#' @examples
labelTumorType<-function(data){
  tumorType<-NULL
  tempTumorType<-NULL
  for (i in 1:nrow(data)){
    tempTumorType<-strsplit(as.character(data$Name)[i],split="-")[[1]][5]
    if(tempTumorType=="NORMAL"){
      tumorType<-append(tumorType,"N")
    }else if(tempTumorType=="OP"){
      tumorType<-append(tumorType,"T2")
    }else{
      tumorType<-append(tumorType,"T1")
    }
  }
  colnames(data)[1]<-"FileName"
  data$TumorType<-tumorType
#   tempFileNames<-NULL
#   for(i in 1:nrow(data)){
#     tempFileNames<-append(tempFileNames,strsplit(as.character(data$FileName[i]),split="/")[[1]][3])
#   }
#   data$FileName<-tempFileNames
  return(data)
}
