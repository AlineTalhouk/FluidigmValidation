#' Title function to check if T2 appears twice in a chunk's tumor type labelling
#'
#' @return
#' @export
#'
#' @examples
test.twiceT2<-function(){
  checkEquals(twiceT2(c("N","T2")),FALSE)
  checkEquals(twiceT2(c("N","T2","T1","T2")),TRUE)
  checkEquals(twiceT2(c("N","T2")),FALSE)
  checkException(twiceT2(1))
  checkException(twiceT2(c("A","T2","T2")))
}
