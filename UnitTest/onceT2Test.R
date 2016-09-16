#' Title function to test FluidigmValidation::onceT2
#'
#' @return
#' @export
#'
#' @examples
test.onceT2<-function(){
  checkEquals(onceT2(c("N","T2")),TRUE)
  checkEquals(onceT2(c("N","N","T2","T1","T2")),FALSE)
  checkEquals(onceT2(c("N","T1")),FALSE)
  checkException(onceT2(1))
  checkException(onceT2(c("A","T2")))
}
