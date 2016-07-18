#' Title function to merge all the xlsx files in a user-input directory
#'
#' @return a data frame of all data merged from the files in the user-defined directory
#' @export
#'
#' @examples
mergeFile<-function(){
  allRawData<-NULL
  dirFiles<-readline(prompt="Please ensure all data files are xlsx files and enter the directory of the files:")
  # confirmDir<-0
  # while(confirmDir!=1){
  #   dirFiles<-readline(prompt="Please enter the directory of the files:")
  #   confirmDir<-readline(prompt="Enter 1 to confirm the directory")
  # }
  setwd(dirFiles)
  myFiles<-list.files()
  rowToStart<-readline(prompt="Please enter the first row of data in all data files: ")

  for(i in 1:length(myFiles)){
    tempData<-read.xlsx(myFiles[i],sheetIndex=1,startRow=rowToStart,header=FALSE)
    tempData$name<-rep(strsplit(myFiles[i],split="[.]")[[1]][1],nrow(tempData))
    allRawData<-rbind(allRawData,tempData)
  }
  allRawData<-allRawData[,c(ncol(allRawData),1:(ncol(allRawData)-1))]
  colnames(allRawData)<-c("Name","#CHROM","POS","ID","REF","ALT","QUAL","FILTER","INFO","FORMAT","NUMS")
  return(allRawData)
}
