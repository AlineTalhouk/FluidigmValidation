filterQualityFrequency<-function(df){
  minQ<-readline(prompt="Enter a minimum quality : ")
  df<-df[df$NUMS$Quality>=as.numeric(minQ),]
  minF<-readline(prompt="Enter a minimum frequency : ")
  df<-df[df$NUMS$frequency>=as.numeric(minF),]
  return(df)
}
