#' Title unit test for FluidigmValidation::checkFC
#'
#' @return
#' @export
#'
#' @examples
test.checkFC<-function(){
  #Load the test data, the test data is based on a real patient
  allData<-read.xlsx(file="/Users/gliu/Documents/FluidigmValidation/UnitTest/checkFCTest/dataForCheckFC.xlsx",sheetName = "Not filtered")
  filtered<-filterQualityFrequency(allData,minQual=80,minFreq=0.03)
  filtered<-splitINFO(filtered)
  checkException(checkFC(subset(filtered,select=-FC)))
  FC_checked<-checkFC(filtered)
  checkEquals(nrow(FC_checked),253)
  checkTrue(abs(mean(FC_checked$POS)-49279015)<1)
  checkTrue(abs(sd(FC_checked$POS)-51489554)<1)
  checkTrue(abs(mean(FC_checked$Frequency)-0.141143)<0.0001)
  checkTrue(abs(sd(FC_checked$Frequency)-0.164569)<0.0001)
}
