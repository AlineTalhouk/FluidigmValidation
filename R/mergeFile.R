#' Title function to merge all the xlsx files in a user-input directory
#'
#' @return a data frame of all data merged from the files in the user-defined directory
#' @export
#'
#' @examples
mergeFile<-function(){
  allRawData<-NULL
  dirFiles<-readline(prompt="Please enter the directory of the files:")
  # confirmDir<-0
  # while(confirmDir!=1){
  #   dirFiles<-readline(prompt="Please enter the directory of the files:")
  #   confirmDir<-readline(prompt="Enter 1 to confirm the directory")
  # }
  setwd(dirFiles)
  myFiles<-list.files()
  fileType<-readline(prompt="Enter 1 if all files are xlsx, 2 if all files are csv, or 3 if all files are txt: ")
  rowToStart<-readline(prompt="Please enter the first row of data in all data files: ")
  for(i in 1:length(myFiles)){
    if(as.character(fileType)=="2"){
      tempData<-read.csv(myFiles[i],skip=as.numeric(rowToStart)-1,header=FALSE)
    }else if(as.character(fileType)=="1"){
      tempData<-read.xlsx(myFiles[i],sheetIndex=1,startRow=rowToStart,header=FALSE)
    }else{
      tempData<-read.table(myFiles[i],skip=as.numeric(rowToStart)-1,header=FALSE)
    }
    tempData$name<-rep(strsplit(myFiles[i],split="[.]")[[1]][1],nrow(tempData))
    allRawData<-rbind(allRawData,tempData)
  }
  allRawData<-allRawData[,c(ncol(allRawData),1:(ncol(allRawData)-1))]
  colnames(allRawData)<-c("Name","#CHROM","POS","ID","REF","ALT","QUAL","FILTER","INFO","FORMAT","NUMS")
  return(allRawData)
}
