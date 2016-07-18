#' Title function to filter out data that do not meet minimum qualiy and frequency requirement
#'
#' @param df : a data frame of data.
#'
#' @return part of input data frame that meet minimum quality and frequency requirements.
#' @export
#'
#' @examples
filterQualityFrequency<-function(df){
  minQ<-readline(prompt="Enter a minimum quality : ")
  df<-df[df$NUMS$Quality>=as.numeric(minQ),]
  minF<-readline(prompt="Enter a minimum frequency : ")
  df<-df[df$NUMS$frequency>=as.numeric(minF),]
  return(df)
}
