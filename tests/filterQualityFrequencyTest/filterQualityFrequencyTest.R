#' Title test function for FluidigimValidation::filterQualityFrequency
#'
#' @return
#' @export
#'
#' @examples
test.filterQualityFreuqency<-function(){
  #Load the data. For testing on another computer other than Johnson Liu's CTAGMAC12, please correct the file path below. The file in use is from
  #PAT-ID82-DNA-ID130-Curettage-A2_S9.txt
  #The same file was processed in Excel by hand and saved as "PAT-ID82-DNA-ID130-Curettage-A2_S9.xls" whose filtered sheet is being tested
  load("/Users/gliu/Documents/FluidigmValidation/UnitTest/filterQualityFrequencyTest/dataForFilterQualityFrequency.rda")
  filtered<-filterQualityFrequency(dataForFilterQualityFrequency,minQual=80,minFreq=0.03)
  checkEquals(nrow(filtered),127)
  checkTrue(abs(mean(filtered$POS)-67607674.3)<1)
  checkTrue(abs(sd(filtered$POS)-51675152.1)<1)
  checkTrue(abs(sd(filtered$Quality)-0)<=0,0)
  checkEquals(mean(filtered$Quality),100)
  checkTrue(abs(mean(filtered$Frequency)-0.08839528)<0.0001)
  checkTrue(abs(sd(filtered$Frequency)-0.13701195)<0.0001)
  checkException(filterQualityFrequency(dataForFilterQualityFrequency,minQual=-1,minFreq=0.03))
  checkException(filterQualityFrequency(dataForFilterQualityFrequency,minQual=101,minFreq=0.03))
  checkException(filterQualityFrequency(dataForFilterQualityFrequency,minQual=80,minFreq=-12))
  checkException(filterQualityFrequency(dataForFilterQualityFrequency,minQual=80,minFreq=1.2))
  checkException(filterQualityFrequency(subset(dataForFilterQualityFrequency,select=-NUMS),minQual=80,minFreq=0.03))
  checkException(filterQualityFrequency(subset(dataForFilterQualityFrequency,select=-FILTER),minQual=80,minFreq=0.03))
}
