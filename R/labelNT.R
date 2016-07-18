#' Title function to label genes according to file name in a data frame of merged data files. If file name contains NORMAL, genes are labeled "N", otherwise,t hey are labeled "T" for tumor
#'
#' @param df : a data frame of all data merged frome several xlsx files
#'
#' @return original data frame with labels
#' @export
#'
#' @examples
labelNT<-function(df){
  labels<-rep("N",nrow(df))
  labels[which(!grepl("NORMAL",df$Name))]<-"T"
  df$labels<-labels
  return(df)
}
