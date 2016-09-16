#' Title function to check if a vector has only 1 T2
#'
#' @param vec : input vector to be checked
#'
#' @return true if vec has only 1 T2 and false otherwise
#' @export
#'
#' @examples
onceT2<-function(vec){
  stopifnot(is.character(vec))
  stopifnot(sum(!unique(vec)%in%c("N","T1","T2"))==0)
  if(sum(vec=="T2")==1){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
