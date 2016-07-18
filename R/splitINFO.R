#' Title function to parse the column INFO by semicolon
#'
#' @param df : data frame of all data
#'
#' @return a data frame whose INFO column has been split into 5 columns: DP, TI, GI, FC, EXON
#' @export
#'
#' @examples
splitINFO<-function(df){
  df$INFO<-colsplit(df$INFO,split=";",names=c("DP","TI","GI","FC","EXON"))
  return(df)
}
