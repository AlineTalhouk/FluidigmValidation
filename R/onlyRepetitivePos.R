#' Title function for finding only data rows whose positions are repetitive
#'
#' @param data whose positions are some repetitive and some not
#' @author Johnson Liu
#' @return part of input data with repetitive positions
#' @export
#'
#' @examples
onlyRepetitivePos<-function(data){
  if(is.null(data$POS)){
    stop("Data does not have position")
  }
  stopifnot(is.numeric(data$POS))
  allPos<-data.frame(data$POS)
  data<-data[(duplicated(allPos) | duplicated(allPos[nrow(allPos):1, ])[nrow(allPos):1]),]
  data<-data[order(data$POS),]
  return(data)
}
