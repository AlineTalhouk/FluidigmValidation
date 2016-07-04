labelNT<-function(df){
  labels<-rep("N",nrow(df))
  labels[which(!grepl("NORMAL",df$Name))]<-"T"
  df$labels<-labels
  return(df)
}
