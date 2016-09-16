#' Title function for testing labelTumorType
#'
#' @return
#' @export
#'
#' @examples
test.labelTumorType<-function(){
  #Create data frame for testing
  Name<-c("PAT-ID37-DNA-ID48-OP-C3_S19.txt","PAT-ID37-DNA-ID48-OP-D3_S20.txt","PAT-ID37-DNA-ID49-Curettage-E3_S21.txt","PAT-ID37-DNA-ID49-Curettage-F3_S22.txt","PAT-ID37-DNA-ID50-NORMAL-G3_S23.txt")
  val<-1:5
  normalDf<-FluidigmValidation::labelTumorType(data.frame(Name,val))
  checkEquals(sum(!normalDf==data.frame(FileName=c("PAT-ID37-DNA-ID48-OP-C3_S19.txt","PAT-ID37-DNA-ID48-OP-D3_S20.txt","PAT-ID37-DNA-ID49-Curettage-E3_S21.txt","PAT-ID37-DNA-ID49-Curettage-F3_S22.txt","PAT-ID37-DNA-ID50-NORMAL-G3_S23.txt")
                                  ,val=1:5
                                  ,TumorType=c("T2","T2","T1","T1","N"))),0)
  Name<-c("PAT-ID37-DNA-ID48-OPS-C3_S19.txt","PAT-ID37-DNA-ID48-OP-D3_S20.txt","PAT-ID37-DNA-ID49-Curettage-E3_S21.txt","PAT-ID37-DNA-ID49-Curettage-F3_S22.txt","PAT-ID37-DNA-ID50-NORMAL-G3_S23.txt")
  checkException(labelTumorType(data.frame(Name,val)))
  Name<-c("PAT-ID37-DNA-ID48-OP","PAT-ID37-DNA-ID48-OP-D3_S20.txt","PAT-ID37-DNA-ID49-Curettage-E3_S21.txt","PAT-ID37-DNA-ID49-Curettage-F3_S22.txt","PAT-ID37-DNA-ID50-NORMAL-G3_S23.txt")
  checkException(labelTumorType(data.frame(Name,val)))
  checkException(labelTumorType(1))
  checkException(labelTumorType("Apple"))
}
