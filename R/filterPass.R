#' Title function to only acquire data rows whose FILTER column is a PASS
#'
#' @param df : a data frame of data
#'
#' @return part of input data frame whose FILTER column is a PASS
#' @export
#'
#' @examples
filterPASS<-function(df){
  df<-df[df$FILTER=="PASS",]
  row.names(df)<-1:nrow(df)
  return(df)
}
