#' Title function to split NUMS column by colon
#'
#' @param df : a data frame
#'
#' @return original data frame with NUMS column split into : "lala","Quality","AllelicRatio","frequency","Unkown1","Unknown2","Unknown3"
#' @export
#'
#' @examples
splitNUMS<-function(df){
  df$NUMS<-colsplit(df$NUMS,split=":",names=c("lala","Quality","AllelicRatio","frequency","Unkown1","Unknown2","Unknown3"))
  return(df)
}
