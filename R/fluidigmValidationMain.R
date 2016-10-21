#' Title main function for processing fluidigm data files
#' @author Johnson Liu
#' @return
#' @export
#'
#' @examples
fluidigmValidationMain<-function(){
  #Get user inputs
  dirFiles<-readline(prompt="Please enter the directory of the files:")
  assert_that(dir.exists(dirFiles))
  checkAllFiles(dirFiles,1)
  #If the operating system is not windows, rename files to windows
  if(Sys.info()["sysname"]!="Windows"){
    renameToTxt(dirFiles)
  }
  minQual<-as.numeric(readline(prompt="Enter the minimum quality number: "))
  minFreq<-as.numeric(readline(prompt="Enter the minimum frequency: "))
  #Create a directory for each patient
  #setwd(dirFiles)
  organize(dirFiles)
  allDirs<-list.dirs(dirFiles,recursive = FALSE) #Get all directories
  tempFiles<-NULL #all files in the directory of a patient
  patientData<-NULL #variable for all useful data of a patient
  tempNUMS<-NULL #varialble for NUMS in a data file
  nameToSave<-NULL #File name to be created (excel file)
  dataFromFile<-NULL # Data read from a txt file
  allData<-NULL
  for(i in 1:length(allDirs)){
    allData<-NULL
    tempFiles<-sort(list.files(allDirs[i],full.names = TRUE))
    tempFilesShort<-sort(list.files(allDirs[i]))
    message(paste("Working on patient",getPatientID(tempFilesShort[1])))
    patientData<-NULL
    #Names for data to be saved
    nameToSave<-paste(allDirs[i],paste(getPatientID(tempFilesShort[1]),"_passedData.rda",sep=""),sep="/")
    excelName<-paste(allDirs[i],paste(getPatientID(tempFilesShort[1]),"_passedData.xlsx",sep=""),sep="/")
    for(j in 1:length(tempFiles)){
      dataFromFile<-read.table(tempFiles[j],header=FALSE)
      colnames(dataFromFile)<-c("CHROM","POS","ID","REF","ALT","QUAL","FILTER","INFO","FORMAT","NUMS")
      dataFromFile$Name<-rep(tempFilesShort[j],nrow(dataFromFile))
      allData<-rbind(allData,dataFromFile)
      #Filter quality and frequency
      dataFromFile<-filterQualityFrequency(data=dataFromFile,minQual=minQual,minFreq=minFreq)

      #Split INFO and only keep EXON observations
      if(nrow(dataFromFile)>0){
        dataFromFile<-splitINFO(dataFromFile)
      }
      #Check for FC requirement
      if(nrow(dataFromFile)>0){
        dataFromFile<-checkFC(dataFromFile)
      }
      #Merge data from file to all data
      if(nrow(dataFromFile)>0){
        patientData<-rbind(patientData,dataFromFile)
      }
    }
    write.xlsx(allData,file=excelName,sheetName="Not filtered",col.names=TRUE,row.names=FALSE, append=TRUE)
    colnames(patientData)[1]<-"FileName"
    save(patientData,file=nameToSave)
    
    #Add step to write all data after all filtering (before taking out repetitive positions and labelling tumor type)
    write.xlsx(patientData,file=excelName,sheetName="filtered with all positions", col.names=TRUE, row.names=FALSE, append=TRUE)
    #Get only the repetitive positions
    patientData<-onlyRepetitivePos(patientData)
    #Label tumor type based on file name
    if(nrow(patientData)>0){
      patientData<-labelTumorType(patientData)
    }
    #Label mutation group (i.e. somatic, germline, normal, or artifact)
    if(nrow(patientData)>0){
      patientData<-labelMutationGroup(patientData)
      write.xlsx(patientData,file=excelName,sheetName="Filtered", col.names = TRUE,row.names = FALSE,append=TRUE)
    }else{
      message(paste("No data after filtering for patient",getPatientID(tempFilesShort[1]),"."))
    }
  }
  message("If there are exceptions, please check all data rows at those positions manually. ")
}
