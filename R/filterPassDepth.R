#' Title function to subset data that has passed filter and minimum depth requirement. Important: do not call this function if you intend to filter according to quality number
#'
#' @param data : original data, filter with PASS and other measures and depths of all values
#' @param minDepth : minimum depth requirement
#'
#' @return data that has passed filter and have quality 100 and at or deeper than the minimum depth
#' @export
#'
#' @examples
filterPassDepth<-function(data,minDepth){
  assert_that(is.data.frame(data))
  assert_that(is.numeric(minDepth))
  assert_that(is.numeric(data$QUAL))
  assert_that(!is.null(data$FILTER))
  assert_that(minDepth>=0)
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
