#' Title function to check if a vector only has 1 "N"
#'
#' @param vec : vector to be checked
#'
#' @return true if the input vector has only 1 "N" and false otherwise
#' @export
#'
#' @examples
onceN<-function(vec){
  stopifnot(is.character(vec))
  stopifnot(sum(!unique(vec)%in%c("N","T1","T2"))==0)
  if(sum(vec=="N")==1){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
