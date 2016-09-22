#' Title unit test function for FluidigmValidation::filterPassDepth
#'
#' @return
#' @export
#'
#' @examples
test.filterPassDepth<-function(){
  pat37<-read.table(file="/Users/gliu/Documents/FluidigmValidation/UnitTest/filterPassDepthTest/PAT-ID37-DNA-ID48-OP-C3_S19.txt",header=FALSE)
  pat140<-read.table(file="/Users/gliu/Documents/FluidigmValidation/UnitTest/filterPassDepthTest/PAT-ID140-DNA-ID229-A1_S1.genome.txt",header=FALSE)
  colnames(pat37)<-c("CHROM","POS","ID",	"REF",	"ALT","QUAL",	"FILTER",	"DEPTH"	,"FORMAT","NUMS")
  colnames(pat140)<-c("CHROM","POS","ID",	"REF",	"ALT","QUAL",	"FILTER",	"DEPTH"	,"FORMAT","NUMS")
  myMinDepth<-1820
  pat37_filtered<-filterPassDepth(pat37,myMinDepth)
  pat140_filtered<-filterPassDepth(pat140,myMinDepth)
}
