#' Title function to subset data that has passed filter and minimum depth requiremtn
#'
#' @param data : original data, filter with PASS and other measures and depths of all values
#' @param minDepth : minimum depth requirement
#'
#' @return data that has passed filter and at or deeper than the minimum depth
#' @export
#'
#' @examples
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
