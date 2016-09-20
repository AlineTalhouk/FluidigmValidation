#' Title function to check if a vector has only 2 T1
#'
#' @param vec : input vector to be checked
#'
#' @return true if vec has only 2 T1 and false otherwise
#' @export
#'
#' @examples
twiceT1<-function(vec){
  assert_that(is.character(vec))
  assert_that(sum(!unique(vec)%in%c("N","T1","T2"))==0)
  if(sum(vec=="T1")==2){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

