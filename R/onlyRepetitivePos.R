#' Title function to find interesting cases (cases with repetitive positions)
#'
#' @param data
#'
#' @return data of only interesting cases
#' @export
#'
#' @examples
onlyRepetitivePos<-function(data){
  allPos<-unique(data$POS)
  for(i in 1:length(allPos)){
    if(length(which(data$POS==allPos[i]))==1){
      data<-data[-which(data$POS==allPos[i]),]
    }
  }
  return(data[order(data$POS),])
}
