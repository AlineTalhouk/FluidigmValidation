splitINFO<-function(df){
  df$INFO<-colsplit(df$INFO,split=";",names=c("DP","TI","GI","FC","EXON"))
  return(df)
}
