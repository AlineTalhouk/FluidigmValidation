#' Title function to check if T1 appears twice in a chunk's tumor type labelling
#'
#' @return
#' @export
#'
#' @examples
test.twiceT1<-function(){
  checkEquals(twiceT1(c("N","T1")),FALSE)
  checkEquals(twiceT1(c("N","N","T1","T1","T2")),TRUE)
  checkEquals(twiceT1(c("N","T2")),FALSE)
  checkException(twiceT1(1))
  checkException(twiceT1(c("A","T1","T1")))
}
