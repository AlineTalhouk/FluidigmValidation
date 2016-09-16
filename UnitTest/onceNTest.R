#' Title function for testing FluidigmValidation::onceN
#'
#' @return
#' @export
#'
#' @examples
test.onceN<-function(){
  checkEquals(onceN(c("N","T1")),TRUE)
  checkEquals(onceN(c("N","N","T1")),FALSE) #It should return false if N has twice.
  checkEquals(onceN(c("T1","T2")),FALSE)
  checkException(onceN(1))
  checkException(onceN(c("N","A","T2")))
}
