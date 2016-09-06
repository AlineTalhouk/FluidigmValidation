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
    tempFiles<-list.files(allDirs[i],full.names = TRUE)
    patientData<-NULL
    #Names for data to be saved
    nameToSave<-paste(allDirs[i],paste(getPatientID(tempFiles[1]),"passedData.rda",sep="-"),sep="/")
    excelName<-paste(allDirs[i],paste(getPatientID(tempFiles[1]),"passedData.xlsx",sep="_"),sep="/")
    for(j in 1:length(tempFiles)){
      dataFromFile<-read.table(tempFiles[j],header=FALSE)
      colnames(dataFromFile)<-c("#CHROM","POS","ID","REF","ALT","QUAL","FILTER","INFO","FORMAT","NUMS")
      dataFromFile$Name<-rep(strsplit(as.character(tempFiles[j]),split="/")[[1]][3],nrow(dataFromFile))
      allData<-rbind(allData,dataFromFile)
      #allData<-cbind(allData$Name,subset(allData,select=-Name))

      #Filter pass only
      dataFromFile<-dataFromFile[dataFromFile$FILTER=="PASS",]
      dataFromFile<-cbind(dataFromFile$Name,subset(dataFromFile,select=-Name))
      colnames(dataFromFile)[1]<-"Name"
      #Split the NUMS
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
    save(patientData,file=nameToSave)
    #Label tumor type based on file name
    patientData<-labelTumorType(patientData)
    #Get only the repetitive positions
    allPos<-data.frame(patientData$POS)
    patientData<-patientData[(duplicated(allPos) | duplicated(allPos[nrow(allPos):1, ])[nrow(allPos):1]),]
    patientData<-patientData[order(patientData$POS),]
    #Label mutation group (i.e. somatic, germline, normal, or artifact)
    if(!is.null(patientData)){
      patientData<-labelMutationGroup(patientData)
      write.xlsx(patientData,file=excelName,sheetName="Filtered", col.names = TRUE,row.names = FALSE,append=TRUE)
    }
  }
  message("If there are exceptions, please check all data rows at those positions manually. ")
}
