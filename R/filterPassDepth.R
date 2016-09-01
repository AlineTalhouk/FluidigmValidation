filterPassDepth<-function(data,minDepth){
  data<-data[data$QUAL==100,]
  data<-data[data$FILTER=="PASS",]
  info<-data$DEPTH
  allInfo<-NULL
  for(i in 1:length(info)){
    tempInfo<-strsplit(as.character(info[i]),split=";")[[1]][1]
    allInfo<-append(allInfo,tempInfo)
  }
  depth<-allInfo
  newDepth<-NULL
  for(i in 1:length(depth)){
    tempDepth<-strsplit(as.character(depth[i]),split="=")[[1]][2]
    newDepth<-append(newDepth,tempDepth)
  }
  data$DEPTH<-as.numeric(newDepth)
  return(data[data$DEPTH>=minDepth,])
}
