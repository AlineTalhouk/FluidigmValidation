processAmpliconInfo<-function(ampliconInfo,data){
  allDepth<-NULL
  chunk<-NULL
  for(i in 1:nrow(ampliconInfo)){
    chunk<-subset(data,POS==ampliconInfo$Position[i]&CHROM==ampliconInfo$Chrom[i])
    if(nrow(chunk)<1){
      allDepth<-append(allDepth,NA)
    }else{
      allDepth<-append(allDepth,paste(chunk$DEPTH,collapse=","))
    }
  }
  ampliconInfo$Depth<-allDepth
  return(t(subset(ampliconInfo,select=c("Region","Depth"))))
}
