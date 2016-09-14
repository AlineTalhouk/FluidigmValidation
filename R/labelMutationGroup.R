#' Title function for labelling mutation group
#'
#' @param data : data with tumor type (i.e. T1, T2, N) labelled
#'
#' @return data with mutation group labelled (i.e. somatic, artifact, germline, normal)
#' @export
#'
#' @examples
labelMutationGroup<-function(data){
  allPos<-unique(data$POS)
  chunk<-NULL
  mutationGroup<-NULL
  for(i in 1:length(allPos)){
    #print(i)
    tempGroup<-NULL
    toAdd<-NULL
    chunk<-data[data$POS==allPos[i],]
    tempGroup<-chunk$TumorType
    if(notMutationGroupException(tempGroup)){
      if(twiceT1(tempGroup) && !twiceT2(tempGroup) && !onceT2(tempGroup) && !onceN(tempGroup)){
        mutationGroup<-append(mutationGroup,rep("Somatic",length(tempGroup)))
        #print("case 1")
      }else if(!twiceT1(tempGroup) && !onceT1(tempGroup) && twiceT2(tempGroup) && !onceN(tempGroup)){
        mutationGroup<-append(mutationGroup,rep("Somatic",length(tempGroup)))
       #print("case 2")
      }else if(onceT1(tempGroup) && onceT2(tempGroup) && !onceN(tempGroup)){
        mutationGroup<-append(mutationGroup,rep("Artifact",length(tempGroup)))
        #print("case 3")
      }else if(onceT1(tempGroup) && twiceT2(tempGroup) && !onceN(tempGroup)){
        toAdd<-rep("NA",length(tempGroup))
        toAdd[which(tempGroup=="T2")]<-"Somatic"
        toAdd[which(tempGroup=="T1")]<-"Artifact"
        mutationGroup<-append(mutationGroup,toAdd)
        #print("case 4")
      }else if(onceT1(tempGroup) && onceT2(tempGroup) && onceN(tempGroup)){
        mutationGroup<-append(mutationGroup,rep("Artifact",length(tempGroup)))
        #print("case 5")
      }else if(onceT1(tempGroup) && twiceT2(tempGroup) && onceN(tempGroup)){
        toAdd<-rep("NA",length(tempGroup))
        toAdd[which(tempGroup=="T2")]<-"Germline"
        toAdd[which(tempGroup=="T1")]<-"Artifact"
        mutationGroup<-append(mutationGroup,toAdd)
        #print("case 6")
      }else if(onceT1(tempGroup) && !onceT2(tempGroup) && !twiceT2(tempGroup) && onceN(tempGroup)){
        mutationGroup<-append(mutationGroup,rep("Artifact",length(tempGroup)))
        #print("case 7")
      }else if(twiceT1(tempGroup) && onceT2(tempGroup) && !onceN(tempGroup)){
        toAdd<-rep("NA",length(tempGroup))
        toAdd[which(tempGroup=="T1")]<-"Somatic"
        toAdd[which(tempGroup=="T2")]<-"Artifact"
        mutationGroup<-append(mutationGroup,toAdd)
        #print("case 8")
      }else if(twiceT1(tempGroup) && twiceT2(tempGroup) && !onceN(tempGroup)){
        mutationGroup<-append(mutationGroup,rep("Somatic",length(tempGroup)))
        #print("case 9")
      }else if(twiceT1(tempGroup) && onceT2(tempGroup) && onceN(tempGroup)){
        toAdd<-rep("NA",length(tempGroup))
        toAdd[which(tempGroup=="T1")]<-"Germline"
        toAdd[which(tempGroup=="T2")]<-"Artifact"
        mutationGroup<-append(mutationGroup,toAdd)
        #print("case 10")
      }else if(twiceT1(tempGroup) && twiceT2(tempGroup) && onceN(tempGroup)){
        mutationGroup<-append(mutationGroup,rep("Germline",length(tempGroup)))
        #print("case 11")
      }else if(twiceT1(tempGroup) && !onceT2(tempGroup) && !twiceT2(tempGroup) && onceN(tempGroup)){
        mutationGroup<-append(mutationGroup,rep("Germline",length(tempGroup)))
      }else if(!onceT1(tempGroup) && !twiceT1(tempGroup) && onceT2(tempGroup) && onceN(tempGroup)){
        mutationGroup<-append(mutationGroup,rep("Artifact",length(tempGroup)))
        #print("case 12")
      }else if(!onceT1(tempGroup) && !twiceT1(tempGroup) && twiceT2(tempGroup) && onceN(tempGroup)){
        mutationGroup<-append(mutationGroup,rep("Germline",length(tempGroup)))
        #print("case 13")
      }else{
        mutationGroup<-append(mutationGroup,rep("Attention",length(tempGroup)))
        #print(paste("Attention please: ",getPatientID(as.character(data[1,1]))))
      }
    }else{
      mutationGroup<-append(mutationGroup,rep("Attention",length(tempGroup)))
      message(paste("Attention please: exception found for patient",getPatientID(as.character(data[1,1])),"Position ",allPos[i]))
      #print("Please check all data rows at those positions manually. ")
    }
  }
  mutationGroup[which(data$TumorType=="N")]<-"NORMAL"
  data$mutationGroup<-mutationGroup
  return(data)
}
