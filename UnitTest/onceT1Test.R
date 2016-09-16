#' Title function to test FluidigmValidation::onceT1
#'
#' @return
#' @export
#'
#' @examples
test.onceT1<-function(){
  checkEquals(onceT1(c("N","T1")),TRUE)
  checkEquals(onceT1(c("N","N","T1","T1","T2")),FALSE)
  checkEquals(onceT1(c("N","T2")),FALSE)
  checkException(onceT1(1))
  checkException(onceT1(c("A","T1")))
}
