#' Title function to check if a vector has only 1 T1
#'
#' @param vec : input vector to be checked
#'
#' @return true if vec has only 1 T1 and false otherwise
#' @export
#'
#' @examples
onceT1<-function(vec){
  if(sum(vec=="T1")==1){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
