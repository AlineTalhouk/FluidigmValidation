#' Title function for testing notMutationGroupException
#'
#' @return
#' @export
#'
#' @examples
test.notMutationGroupException<-function(){
  checkEquals(notMutationGroupException(c("T1","T1")),TRUE)
  checkEquals(notMutationGroupException(c("T2","T2")),TRUE)
  checkEquals(notMutationGroupException(c("T1","T2")),TRUE)
  checkEquals(notMutationGroupException(c("T1","T2","T2")),TRUE)
  checkEquals(notMutationGroupException(c("T1","T2","N")),TRUE)
  checkEquals(notMutationGroupException(c("T1","T2","T2","N")),TRUE)
  checkEquals(notMutationGroupException(c("T1","N")),TRUE)
  checkEquals(notMutationGroupException(c("T1","T1","T2")),TRUE)
  checkEquals(notMutationGroupException(c("T1","T1","T2","T2")),TRUE)
  checkEquals(notMutationGroupException(c("T1","T1","T2","N")),TRUE)
  checkEquals(notMutationGroupException(c("T1","T1","T2","T2","N")),TRUE)
  checkEquals(notMutationGroupException(c("T1","T1","N")),TRUE)
  checkEquals(notMutationGroupException(c("T2","N")),TRUE)
  checkEquals(notMutationGroupException(c("T2","T2","N")),TRUE)
  checkEquals(notMutationGroupException(c("N","N")),FALSE)
  checkEquals(notMutationGroupException(c("T1","T1","T1","N")),FALSE)
  checkEquals(notMutationGroupException(c("T2","N","T2","T2")),FALSE)
  checkException(notMutationGroupException(23))
  checkException(notMutationGroupException(c("A","N","T1","T2")))
}
