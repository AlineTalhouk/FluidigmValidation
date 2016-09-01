notMutationGroupException<-function(vec){
  if(sum(vec=="N")>1){
    return(FALSE)
  }else if(sum(vec=="T1")>2){
    return(FALSE)
  }else if(sum(vec=="T2")>2){
    return(FALSE)
  }else{
    return(TRUE)
  }
}
