passSummary<-function(){
  oldDir<-getwd()
  dirFiles<-readline(prompt="Please enter the directory of the files:")
  minRows<-as.numeric(readline(prompt="Enter the minimum number of rows for the file to be labelled yes: "))
  minDepth<-as.numeric(readline(prompt="Enter the minimum depth: "))
  setwd(dirFiles)
  organize(dirFiles)
  filteredSheetName<-NULL
  allFileNames<-NULL
  allFileRows<-NULL
  allFilePass<-NULL
  tempSummary<-NULL
  allDirs<-list.dirs(recursive = FALSE) #Get all directories
  if(length(allDirs)<1){
    stop("Nothing in directory for passSummary")
  }
  tempFiles<-NULL #all files in the directory of a patient
  for(i in 1:length(allDirs)){
    tempFiles<-list.files(allDirs[i])
    for(j in 1:length(tempFiles)){
      allFileNames<-append(allFileNames,tempFiles[j])
      excelName<-paste(tempFiles[j],".xlsx",sep="")
      excelName<-paste(allDirs[i],excelName,sep="/")
      dataFromFile<-read.table(paste(allDirs[i],tempFiles[j],sep="/"),header = FALSE)
      names(dataFromFile)<-c("#CHROME","POS","ID",	"REF",	"ALT","QUAL",	"FILTER",	"DEPTH"	,"FORMAT","NUMS")
      write.xlsx(dataFromFile,file=excelName,sheet="Original",col.names=TRUE,row.names = FALSE)
      dataFromFile<-filterPassDepth(dataFromFile,minDepth)
      if(nrow(dataFromFile)>=minRows){
        filteredSheetName<-"filtered-Y"
        allFilePass<-append(allFilePass,"Yes")
      }else{
        filteredSheetName<-"filtered-N"
        allFilePass<-append(allFilePass,"No")
      }
      write.xlsx(dataFromFile,file=excelName,sheetName =filteredSheetName,append=TRUE,row.names = FALSE,col.names<-TRUE)
      allFileRows<-append(allFileRows,nrow(dataFromFile))
    }
  }
  setwd(oldDir)
  write.xlsx(data.frame(fileName=allFileNames,NumberOfRows=allFileRows,PassOrNot=allFilePass),file="passSummary.xlsx",row.names = FALSE,col.names = TRUE)
}
