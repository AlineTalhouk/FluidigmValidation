#' Title function to summarize raw genome files by taking only data that has passed filter and has enough depth
#'
#' @return
#' @export
#' @author Johnson Liu
#' @examples
passSummary<-function(){
  dirFiles<-readline(prompt="Please enter the directory of the files:")
  assert_that(dir.exists(dirFiles))
  checkAllFiles(dirFiles,2)
  if(Sys.info()["sysname"]!="Windows"){
    renameToTxt(dirFiles)
  }
  #minRows<-as.numeric(readline(prompt="Enter the minimum number of rows for the file to be labelled pass: "))
  #minDepth<-as.numeric(readline(prompt="Enter the minimum depth: "))
  ampliconInfo<-read.xlsx(paste(dirFiles,"/Mutation for every amplicons.xls",sep=""),sheetIndex = 1,header=TRUE)
  organize(dirFiles)
  filteredSheetName<-NULL
  allFileNames<-NULL
  allFileRows<-NULL
  allFilePass<-NULL
  allAmpliconInfo<-NULL
  tempSummary<-NULL
  fileAmplicon<-NULL
  #List all the directories
  allDirs<-list.dirs(dirFiles,recursive = FALSE) #Get all directories
  if(length(allDirs)<1){
    stop("Nothing in directory for passSummary")
  }
  tempFiles<-NULL #all files in the directory of a patient
  tempFilesShort<-NULL
  for(i in 1:length(allDirs)){
    tempFiles<-sort(list.files(allDirs[i],full.names = TRUE))
    tempFilesShort<-sort(list.files(allDirs[i]))
    message(paste("Working on patient",getPatientID(tempFilesShort[1]),"."))
    for(j in 1:length(tempFiles)){
      allFileNames<-append(allFileNames,tempFilesShort[j])
      excelName<-paste(tempFiles[j],".xlsx",sep="")
      dataFromFile<-read.table(tempFiles[j],header = FALSE)
      colnames(dataFromFile)<-c("CHROM","POS","ID",	"REF",	"ALT","QUAL",	"FILTER",	"DEPTH"	,"FORMAT","NUMS")

      dataForAmpliconFilter<-filterPassDepth(dataFromFile,0)
      if(is.null(dataForAmpliconFilter)){
        message(paste(tempFilesShort[j],"has no data after pass filtering, quality 100 filtering, and depth 0 filtering"))
        allAmpliconInfo<-rbind(allAmpliconInfo,rep("-1",ncol(allAmpliconInfo)))
      }else{
        fileAmplicon<-processAmpliconInfo(ampliconInfo,dataForAmpliconFilter)
        allAmpliconInfo<-rbind(allAmpliconInfo,fileAmplicon[2,])
      }
    }
  }
  colnames(allAmpliconInfo)<-ampliconInfo$Region
  rownames(allAmpliconInfo)<-allFileNames

  write.xlsx(allAmpliconInfo,file=paste(dirFiles,"passSummary.xlsx",sep="/"),row.names = TRUE,col.names = TRUE,sheetName="AmpliconInfo",append=TRUE)
}
