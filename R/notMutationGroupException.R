#' Title function to check if a chunk is an mutation group exception (e.g have more than 1 N, or more than 2 T1 or T2)
#'
#' @param vec : input vector for tumor type
#' @author Johnson Liu
#' @return TRUE is it is exception false otherwise
#' @export
#'
#' @examples
notMutationGroupException<-function(vec){
  assert_that(is.character(vec))
  assert_that(sum(!unique(vec)%in%c("N","T1","T2"))==0)
  if(sum(vec=="N")>1){
    return(FALSE)
  }else if(sum(vec=="T1")>2){
    return(FALSE)
  }else if(sum(vec=="T2")>2){
    return(FALSE)
  }else{
    return(TRUE)
  }
}
