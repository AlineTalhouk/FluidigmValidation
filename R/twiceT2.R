#' Title function to check if a vector has only 2 T2
#'
#' @param vec : input vector to be checked
#'
#' @return true if vec has only 2 T2 and false otherwise
#' @export
#'
#' @examples
twiceT2<-function(vec){
  if(sum(vec=="T2")==2){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
