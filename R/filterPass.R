filterPASS<-function(df){
  return(df[which(df$FILTER=="PASS"),])
}
