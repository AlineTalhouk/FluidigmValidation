#' Title function to summarize raw genome files by taking only data that has passed filter and has enough depth
#'
#' @return
#' @export
#'
#' @examples
passSummary<-function(){
  #oldDir<-getwd()
  dirFiles<-readline(prompt="Please enter the directory of the files:")
  minRows<-as.numeric(readline(prompt="Enter the minimum number of rows for the file to be labelled yes: "))
  minDepth<-as.numeric(readline(prompt="Enter the minimum depth: "))
  #setwd(dirFiles)
  organize(dirFiles)
  filteredSheetName<-NULL
  allFileNames<-NULL
  allFileRows<-NULL
  allFilePass<-NULL
  tempSummary<-NULL
  #List all the directories
  allDirs<-list.dirs(dirFiles,recursive = FALSE) #Get all directories
  if(length(allDirs)<1){
    stop("Nothing in directory for passSummary")
  }
  tempFiles<-NULL #all files in the directory of a patient
  for(i in 1:length(allDirs)){
    tempFiles<-list.files(allDirs[i],full.names = TRUE)
    for(j in 1:length(tempFiles)){
      allFileNames<-append(allFileNames,strsplit(as.character(tempFiles[j]),split="/")[[1]][3])
      excelName<-paste(tempFiles[j],".xlsx",sep="")
      #excelName<-paste(allDirs[i],excelName,sep="/")
      dataFromFile<-read.table(tempFiles[j],header = FALSE)
      colnames(dataFromFile)<-c("#CHROM","POS","ID",	"REF",	"ALT","QUAL",	"FILTER",	"DEPTH"	,"FORMAT","NUMS")
      #Write unfiltered, raw data to a separate excel file
      write.xlsx(dataFromFile,file=excelName,sheet="Original",col.names=TRUE,row.names = FALSE)
      #Filter data based on filter and depth
      dataFromFile<-filterPassDepth(dataFromFile,minDepth)
      #Label each file as passed or failed based on number of rows
      if(nrow(dataFromFile)>=minRows){
        filteredSheetName<-"filtered-Y"
        allFilePass<-append(allFilePass,"Passed")
      }else{
        filteredSheetName<-"filtered-N"
        allFilePass<-append(allFilePass,"Failed")
      }
      #Write filtered data to the excel file for that patient
      write.xlsx(dataFromFile,file=excelName,sheetName =filteredSheetName,append=TRUE,row.names = FALSE,col.names<-TRUE)
      allFileRows<-append(allFileRows,nrow(dataFromFile))
    }
  }
  #setwd(oldDir)
  #Ouput all data to another excel sheet
  write.xlsx(data.frame(fileName=allFileNames,NumberOfRows=allFileRows,PassOrNot=allFilePass),file=paste(dirFiles,"passSummary.xlsx",sep="\\"),row.names = FALSE,col.names = TRUE)
}
