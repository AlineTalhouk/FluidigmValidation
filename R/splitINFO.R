#' Title function to split INFO column in fluidigm data
#'
#' @param data with INFO column not splited
#' @author Johnson Liu
#' @return data with INFO columns splited into DP, TI, GI, FC and only data with EXON
#' @export
#'
#' @examples
splitINFO<-function(data){
  vec<-NULL
  DP<-NULL
  TI<-NULL
  GI<-NULL
  FC<-NULL
  EXON<-NULL
  if(is.null(data$INFO)){
    stop("Data does not have INFO to be splited.")
  }
  for(i in 1:nrow(data)){
    vec<-strsplit(as.vector(data$INFO[i]),split=";")[[1]]
    vec<-strsplit(vec,split="=")
    stopifnot(length(vec)%in%c(4,5))
    DP<-append(DP,vec[[1]][2])
    TI<-append(TI,vec[[2]][2])
    GI<-append(GI,vec[[3]][2])
    FC<-append(FC,vec[[4]][2])
    if(length(vec)==4){
      EXON<-append(EXON,"NO")
    }else{
      EXON<-append(EXON,"YES")
    }
  }
  data<-subset(data,select=-INFO)
  data<-cbind(data,DP,TI,GI,FC,EXON)
  data<-data[data$EXON=="YES",]
  return(data)
}
