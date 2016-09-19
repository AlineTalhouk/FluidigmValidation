#' Title function for unit testing of FluidigmValidation::splitINFO
#'
#' @return
#' @export
#'
#' @examples
test.splitINFO<-function(){
  #Load the data. For testing on another computer other than Johnson Liu's CTAGMAC12, please correct the file path below. The file in use is from
  #PAT-ID82-DNA-ID130-Curettage-A2_S9.txt
  #The same file was processed in Excel by hand and saved as "PAT-ID82-DNA-ID130-Curettage-A2_S9.xls" whose filtered sheet is being tested
  load("/Users/gliu/Documents/FluidigmValidation/UnitTest/filterQualityFrequencyTest/dataForFilterQualityFrequency.rda")
  filtered<-filterQualityFrequency(dataForFilterQualityFrequency,minQual=80,minFreq=0.03)
  checkException(splitINFO(subset(filtered,select=-INFO)))
  #splitINFO usually should occur after quality and frequency have been filtered
  filtered<-splitINFO(filtered)

  #Check split info processes
  checkException(splitINFO(subset(filtered,select=-NUMS)))
  checkEquals(nrow(filtered),91)
  checkEquals(filtered$POS[48],133253167)
  checkEquals(filtered$Frequency[91],0.0537)

  #Check by computations
  checkTrue(abs(mean(filtered$POS)-60454964.6)<1)
  checkTrue(abs(sd(filtered$POS)-54260560.8)<1)
  checkTrue(abs(mean(filtered$Frequency)-0.07722637)<0.0001)
  checkTrue(abs(sd(filtered$Frequency)-0.11372661)<0.0001)

  #Check if INFO has been splited into 4(no EXON) or 5(with EXON) parts correctly. Should be just 4 or 5 parts, no other numbers of parts
  #This data has INFO column in the first row having an extra term (6 terms in total)
  data_tooMuchInfo<-read.xlsx(file="/Users/gliu/Documents/FluidigmValidation/UnitTest/filterQualityFrequencyTest/PAT-ID82-DNA-ID130-Curettage-A2_S9.xls",sheetName = "INFOSplited_errorInFactors")
  checkException(splitINFO(data_tooMuchInfo))
}
