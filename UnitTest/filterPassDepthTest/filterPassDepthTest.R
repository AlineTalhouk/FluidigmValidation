#' Title unit test function for FluidigmValidation::filterPassDepth
#'
#' @return
#' @export
#'
#' @examples
test.filterPassDepth<-function(){
  pat37<-read.table(file="/Users/gliu/Documents/FluidigmValidation/UnitTest/filterPassDepthTest/PAT-ID37-DNA-ID48-OP-C3_S19.txt",header=FALSE)
  colnames(pat37)<-c("CHROM","POS","ID",	"REF",	"ALT","QUAL",	"FILTER",	"DEPTH"	,"FORMAT","NUMS")
  myMinDepth<-1820
  pat37_filtered<-filterPassDepth(pat37,myMinDepth)
  checkEquals(nrow(pat37_filtered),98)
  checkTrue(abs(mean(pat37_filtered$POS)-72754391.8)<1)
  checkTrue(abs(sd(pat37_filtered$POS)-56691671.4)<1)
  checkTrue(abs(mean(pat37_filtered$DEPTH)-4788.44898)<1)
  checkTrue(abs(sd(pat37_filtered$DEPTH)-1525.05233)<1)
  checkException(filterPassDepth("data",15))
  checkException(filterPassDepth(data=12,"depth"))
  checkException(filterPassDepth(subset(pat37,select=-QUAL)))
  checkException(filterPassDepth(subset(pat37,select=-DEPTH)))
  checkException(filterPassDepth(subset(pat37,select=-FILTER)))

}
