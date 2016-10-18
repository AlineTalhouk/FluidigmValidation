#' Title function for labelling mutation group
#'
#' @param data : data with tumor type (i.e. T1, T2, N) labelled
#' @author Johnson Liu
#' @return data with mutation group labelled (i.e. somatic, artifact, germline, normal) and only whose positions are repetitive
#' @export
#'
#' @examples
labelMutationGroup<-function(data){
  if(!is.numeric(data$POS)){
    stop("Positions in data are not numeric or data is empty.")
  }
  if(is.null(data$TumorType)){
    stop("Tumor type is not labelled. ")
  }
  allPos<-unique(data$POS)
  chunk<-NULL
  mutationGroup<-NULL
  for(i in 1:length(allPos)){
    #print(i)
    tempGroup<-NULL
    toAdd<-NULL
    chunk<-data[data$POS==allPos[i],]
    assert_that(nrow(chunk)>1)
    tempGroup<-chunk$TumorType
    if(notMutationGroupException(tempGroup)){
      if(twiceT1(tempGroup) && !twiceT2(tempGroup) && !onceT2(tempGroup) && !onceN(tempGroup)){
        mutationGroup<-append(mutationGroup,rep("Somatic",length(tempGroup)))
        print(paste("Position",allPos[i],"case 1"))
      }else if(!twiceT1(tempGroup) && !onceT1(tempGroup) && twiceT2(tempGroup) && !onceN(tempGroup)){
        mutationGroup<-append(mutationGroup,rep("Somatic",length(tempGroup)))
        print(paste("Position",allPos[i],"case 2"))
      }else if(onceT1(tempGroup) && onceT2(tempGroup) && !onceN(tempGroup)){
        mutationGroup<-append(mutationGroup,rep("Artifact",length(tempGroup)))
        print(paste("Position",allPos[i],"case 3"))
      }else if(onceT1(tempGroup) && twiceT2(tempGroup) && !onceN(tempGroup)){
        toAdd<-rep("NA",length(tempGroup))
        toAdd[which(tempGroup=="T2")]<-"Somatic"
        toAdd[which(tempGroup=="T1")]<-"Artifact"
        mutationGroup<-append(mutationGroup,toAdd)
        print(paste("Position",allPos[i],"case 4"))
      }else if(onceT1(tempGroup) && onceT2(tempGroup) && onceN(tempGroup)){
        toAdd<-rep("Artifact",length(tempGroup));
        toAdd[which(tempGroup=="N")]<-"Normal"
        mutationGroup<-append(mutationGroup,toAdd)
        print(paste("Position",allPos[i],"case 5"))
      }else if(onceT1(tempGroup) && twiceT2(tempGroup) && onceN(tempGroup)){
        toAdd<-rep("NA",length(tempGroup))
        toAdd[which(tempGroup=="T2")]<-"Germline"
        toAdd[which(tempGroup=="T1")]<-"Artifact"
        toAdd[which(tempGroup=="N")]<-"Normal"
        mutationGroup<-append(mutationGroup,toAdd)
        print(paste("Position",allPos[i],"case 6"))
      }else if(onceT1(tempGroup) && !onceT2(tempGroup) && !twiceT2(tempGroup) && onceN(tempGroup)){
        toAdd<-rep("Artifact",length(tempGroup))
        toAdd[which(tempGroup=="N")]<-"Normal"
        mutationGroup<-append(mutationGroup,toAdd)
        print(paste("Position",allPos[i],"case 7"))
      }else if(twiceT1(tempGroup) && onceT2(tempGroup) && !onceN(tempGroup)){
        toAdd<-rep("NA",length(tempGroup))
        toAdd[which(tempGroup=="T1")]<-"Somatic"
        toAdd[which(tempGroup=="T2")]<-"Artifact"
        mutationGroup<-append(mutationGroup,toAdd)
        print(paste("Position",allPos[i],"case 8"))
      }else if(twiceT1(tempGroup) && twiceT2(tempGroup) && !onceN(tempGroup)){
        mutationGroup<-append(mutationGroup,rep("Somatic",length(tempGroup)))
        print(paste("Position",allPos[i],"case 9"))
      }else if(twiceT1(tempGroup) && onceT2(tempGroup) && onceN(tempGroup)){
        toAdd<-rep("NA",length(tempGroup))
        toAdd[which(tempGroup=="T1")]<-"Germline"
        toAdd[which(tempGroup=="T2")]<-"Artifact"
        toAdd[which(tempGroup=="N")]<-"Normal"
        mutationGroup<-append(mutationGroup,toAdd)
        print(paste("Position",allPos[i],"case 10"))
      }else if(twiceT1(tempGroup) && twiceT2(tempGroup) && onceN(tempGroup)){
        toAdd<-rep("Germline",length(tempGroup))
        toAdd[which(tempGroup=="N")]<-"Normal"
        mutationGroup<-append(mutationGroup,toAdd)
        print(paste("Position",allPos[i],"case 11"))
      }else if(twiceT1(tempGroup) && !onceT2(tempGroup) && !twiceT2(tempGroup) && onceN(tempGroup)){
        toAdd<-rep("Germline",length(tempGroup))
        toAdd[which(tempGroup=="N")]<-"Normal"
        mutationGroup<-append(mutationGroup,toAdd)
        print(paste("Position",allPos[i],"case 12"))
      }else if(!onceT1(tempGroup) && !twiceT1(tempGroup) && onceT2(tempGroup) && onceN(tempGroup)){
        toAdd<-rep("Artifact",length(tempGroup))
        toAdd[which(tempGroup=="N")]<-"Normal"
        mutationGroup<-append(mutationGroup,toAdd)
        print(paste("Position",allPos[i],"case 13"))
      }else if(!onceT1(tempGroup) && !twiceT1(tempGroup) && twiceT2(tempGroup) && onceN(tempGroup)){
        toAdd<-rep("Germline",length(tempGroup))
        toAdd[which(tempGroup=="N")]<-"Normal"
        mutationGroup<-append(mutationGroup,toAdd)
        print(paste("Position",allPos[i],"case 14"))
      }
    }else{
      mutationGroup<-append(mutationGroup,rep("Attention",length(tempGroup)))
      message(paste("Attention please: exception found for patient",getPatientID(as.character(data$FileName[1])),"Position ",allPos[i]))
      message(paste("There are",length(tempGroup),"rows of data at this position"))
    }
  }
  #mutationGroup[which(data$TumorType=="N")]<-"NORMAL"
  data$mutationGroup<-mutationGroup
  return(data)
}
