#' Title function to filter quality and frequency for fluidigmValidationMain
#'
#' @param data : data with columns FILTER and NUMS from original fluidigm machine fiels
#' @param minQual : minimum quality number entered by user
#' @param minFreq : minimum frequency entered by user
#'
#' @return modified data with filter only equal to PASS and those that meet minimum frequency and quality requirements.
#' @export
#'
#' @examples
filterQualityFrequency<-function(data,minQual,minFreq){
  #Check ranges of minQual and minFreq
  assert_that(is.numeric(minQual))
  assert_that(is.numeric(minFreq))
  assert_that(minQual<=100 && minQual>=0)
  assert_that(minFreq>=0 && minFreq<=1)
  #Check if data has FILTER and NUMS
  if(is.null(data$FILTER)){
    stop("Data does not have FILTER for function filterQualityFrequency.")
  }
  if(is.null(data$NUMS)){
    stop("Data does not have NUMS for function filterQualityFrequency.")
  }
  data<-data[data$FILTER=="PASS",] #Only PASS in FILTER
  #Put name to the leftmost column
  data<-cbind(data$Name,subset(data,select=-Name))
  colnames(data)[1]<-"Name"
  #Split NUMS
  if(nrow(data)>0){
    tempNUMS<-data.frame(colsplit(data$NUMS,split=":",names=c("Unkown1","Quality","AllelicRatio","Frequency","Unkown2","Unknown3","Unknown4")))
    data<-cbind(subset(data,select=-NUMS),tempNUMS)
    #Filter quality
    data<-data[data$Quality>=minQual,]
    #filter frequency
    data<-data[data$Frequency>=minFreq,]
  }
  return(data)
}
