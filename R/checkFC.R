#' Title function to check FC
#'
#' @param data : data with INFO column splited
#'
#' @return data that met FC criteria that rows with only Silent_something, Silent, Synonymous_something, Synonymous are deleted
#' @export
#'
#' @examples
checkFC<-function(data){
  vec<-NULL #vector for a string of FC values before parsing
  tempTypes<-NULL #variable for temporary FC values
  FC_label<-NULL #variable for storing whether a row pass or not based on FC
  if(is.null(data$FC)){
    stop("Data does not have FC")
  }
  for(i in 1:nrow(data)){
    tempTypes<-NULL
    vec<-strsplit(as.vector(data$FC[i]),split=",")[[1]]
    vec<-strsplit(vec,split="_")
    for(j in 1:length(vec)){
      tempTypes<-append(tempTypes,vec[[j]][1])
    }
    #Only data rows with not just silient and/or synonymous
    if(sum(!(unique(tempTypes)%in%c("Silent","Synonymous")))!=0){
      FC_label<-append(FC_label,"FC_pass")
    }else{
      FC_label<-append(FC_label,"FC_fail")
    }
  }
  data<-cbind(data,FC_label)
  data<-data[data$FC_label=="FC_pass",]
  return(subset(data,select=-FC_label))
}
