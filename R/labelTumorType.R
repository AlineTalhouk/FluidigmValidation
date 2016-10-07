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
  assert_that(is.data.frame(data))
  assert_that(!is.null(data$FileName))
  tumorType<-NULL
  tempTumorType<-NULL
  for (i in 1:nrow(data)){
    tempTumorType<-strsplit(as.character(data$FileName)[i],split="-")[[1]][5]
    if(tempTumorType=="NORMAL"){
      tumorType<-append(tumorType,"N")
    }else if(tempTumorType=="OP"){
      tumorType<-append(tumorType,"T2")
    }else if(tempTumorType=="Curettage"){
      tumorType<-append(tumorType,"T1")
    }else{
      stop(paste("Cannot get tumor type from file:",data$Name[i],". Please check file name"))
    }
  }
  data$TumorType<-tumorType
  return(data)
}
