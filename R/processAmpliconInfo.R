#' Title function to perform amplicon information
#'
#' @param ampliconInfo : excel sheet for amplicon info, titled "Mutation for every amplicons.xls"
#' @param data : data from filterPassDepth with depth setting 0
#'
#' @return data with depth for each amplicon from each file
#' @export
#'
#' @examples
processAmpliconInfo<-function(ampliconInfo,data){
  assert_that(is.data.frame(ampliconInfo))
  assert_that(is.data.frame(data))
  assert_that(is.numeric(ampliconInfo$Position))
  assert_that(is.numeric(data$POS))
  allDepth<-NULL
  chunk<-NULL
  ampliconInfo$Position<-ceiling(ampliconInfo$Position)
  for(i in 1:nrow(ampliconInfo)){
    chunk<-subset(data,POS==ampliconInfo$Position[i]&CHROM==ampliconInfo$Chrom[i])
    if(nrow(chunk)<1){
      allDepth<-append(allDepth,NA)
    }else{
      allDepth<-append(allDepth,paste(chunk$DEPTH,collapse=","))
    }
  }
  ampliconInfo$Depth<-allDepth
  return(t(subset(ampliconInfo,select=c("Region","Depth"))))
}
