# allRawData<-NULL
# myFiles<-list.files()
# for(i in 1:length(myFiles)){
#   tempWB<-loadWorkbook(filename=myFiles[i])
#   tempData<-readWorksheet(tempWB,sheet=1,startRow=29,header=FALSE)
#   tempData$name<-rep(strsplit(myFiles[i],split="[.]")[[1]][1],nrow(tempData))
#   head(tempData)
#   allRawData<-rbind(allRawData,tempData)
# }
allRawData<-NULL
dirFiles<-readline(prompt="Please enter the directory of the files:")
# confirmDir<-0
# while(confirmDir!=1){
#   dirFiles<-readline(prompt="Please enter the directory of the files:")
#   confirmDir<-readline(prompt="Enter 1 to confirm the directory")
# }
setwd(dirFiles)
myFiles<-list.files()

for(i in 1:length(myFiles)){
  tempWB<-loadWorkbook(file=myFiles[i])
  tempData<-read.xlsx(myFiles[i],sheetIndex=1,startRow=29,header=FALSE)
  tempData$name<-rep(strsplit(myFiles[i],split="[.]")[[1]][1],nrow(tempData))
  allRawData<-rbind(allRawData,tempData)
}
allRawData<-allRawData[,c(ncol(allRawData),1:(ncol(allRawData)-1))]
colnames(allRawData)<-c("Name","#CHROM","POS","ID","REF","ALT","QUAL","FILTER","INFO","FORMAT","NUMS")
