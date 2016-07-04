splitNUMS<-function(df){
  df$NUMS<-colsplit(df$NUMS,split=":",names=c("lala","Quality","AllelicRatio","frequency","Unkown1","Unknown2","Unknown3"))
  return(df)
}
