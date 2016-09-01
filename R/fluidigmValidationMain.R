#' Title main function for processing fluidigm data files
#'
#' @return
#' @export
#'
#' @examples
fluidigmValidationMain<-function(){
  #Get user inputs
  oldDir<-getwd()
  dirFiles<-readline(prompt="Please enter the directory of the files:")
  minQual<-as.numeric(readline(prompt="Enter the minimum quality number: "))
  minFreq<-as.numeric(readline(prompt="Enter the minimum frequency: "))
  #Create a directory for each patient
  setwd(dirFiles)
  organize(dirFiles)
  allDirs<-list.dirs(recursive = FALSE) #Get all directories
  tempFiles<-NULL #all files in the directory of a patient
  patientData<-NULL #variable for all useful data of a patient
  tempNUMS<-NULL #varialble for NUMS in a data file
  nameToSave<-NULL #File name to be created (excel file)
  dataFromFile<-NULL # Data read from a txt file
  allData<-NULL
  for(i in 1:length(allDirs)){
    allData<-NULL
    tempFiles<-list.files(allDirs[i])
    patientData<-NULL
    #Names for data to be saved
    nameToSave<-paste(allDirs[i],paste(getPatientID(tempFiles[1]),"passedData.rda",sep="-"),sep="/")
    excelName<-paste(allDirs[i],paste(getPatientID(tempFiles[1]),"passedData.xlsx",sep="_"),sep="/")
    for(j in 1:length(tempFiles)){
      dataFromFile<-read.table(paste(allDirs[i],tempFiles[j],sep="/"),header=FALSE)
      names(dataFromFile)<-c("#CHROM","POS","ID","REF","ALT","QUAL","FILTER","INFO","FORMAT","NUMS")
      dataFromFile$Name<-rep(tempFiles[j],nrow(dataFromFile))
      allData<-rbind(allData,dataFromFile)
      #allData<-cbind(allData$Name,subset(allData,select=-Name))

      #Filter pass only
      dataFromFile<-dataFromFile[dataFromFile$FILTER=="PASS",]
      dataFromFile<-cbind(dataFromFile$Name,subset(dataFromFile,select=-Name))
      colnames(dataFromFile)[1]<-"Name"
      tempNUMS<-data.frame(colsplit(dataFromFile$NUMS,split=":",names=c("Unkown1","Quality","AllelicRatio","frequency","Unkown2","Unknown3","Unknown4")))
      dataFromFile<-cbind(subset(dataFromFile,select=-NUMS),tempNUMS)
      #Filter quality
      dataFromFile<-dataFromFile[dataFromFile$Quality>=minQual,]
      #filter frequency
      dataFromFile<-dataFromFile[dataFromFile$frequency>=minFreq,]
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
    colnames(patientData)[1]<-"Name"
    patientData<-labelTumorType(patientData)
    allPos<-data.frame(patientData$POS)
    patientData<-patientData[(duplicated(allPos) | duplicated(allPos[nrow(allPos):1, ])[nrow(allPos):1]),]
    patientData<-patientData[order(patientData$POS),]
    if(!is.null(patientData)){
      patientData<-labelMutationGroup(patientData)
      save(patientData,file=nameToSave)
      write.xlsx(patientData,file=excelName,sheetName="Filtered", col.names = TRUE,row.names = FALSE,append=TRUE)
    }
  }
  setwd(oldDir)
  message("Please check all data rows at those positions manually. ")
}
