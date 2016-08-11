#' Title Main function to perform analysis of Fluidigm data sets
#'
#' @return an excel file of data labeled, filtered, and only cases with repetitive pos values
#' @export
#'
#' @examples
fluidigmValidationMain<-function(){
  merged<-mergeFile()
  allRawData<-merged$allRawData
  allRawFiles<-merged$allRawFiles
  labeledData<-labelNT(allRawData)
  passedData<-filterPASS(labeledData)
  data_INFO_splited<-splitINFO(passedData)
  data_NUMS_splited<-splitNUMS(data_INFO_splited)
  data_QF_filtered<-filterQualityFrequency(data_NUMS_splited)
  pureINFO<-data_QF_filtered$INFO
  colnames(pureINFO)<-c("DP","TI","GI","FC",'EXON')
  pureNUMS<-data_QF_filtered$NUMS
  colnames(pureNUMS)<-c("lala","quality","alleicratio","frequency", "U1","U2","U3")
  allData<-subset(data_QF_filtered,select=-INFO)
  allData<-subset(allData,select=-NUMS)
  allData<-cbind(allData,pureINFO,pureNUMS)
  write.xlsx(allData,file="allOutput.xlsx",row.names = FALSE,sheetName = "allOutput")
  allRepetitiveData<-onlyRepetitivePos(allData)
  write.xlsx(allRepetitiveData,file="allOutput.xlsx",row.names = FALSE,sheetName = "allRepetitivePositions",append=TRUE)
  moreFiles(allRepetitiveData)
  if(length(allRawFiles)<1){
    stop("No raw files")
  }else{
    for(i in 1:length(allRawFiles)){
      file.copy(allRawFiles[i],paste(".\\",getPatientID(allRawFiles[i]),"\\",allRawFiles[i],sep=""))
      file.remove(allRawFiles[i])
    }
  }
}
