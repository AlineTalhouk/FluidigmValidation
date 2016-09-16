#' Title function to check if files in user-input directory are suitable for fluidigmValidationMain or passSummary
#'
#' @param dirFiles : directory of files
#' @param funInd : 1 for fluidigmValidationMain and 2 for passSummary
#'
#' @return
#' @export
#'
#' @examples
checkAllFiles<-function(dirFiles,funInd){
  stopifnot(funInd %in% c(1,2))
  allFiles<-list.files(dirFiles)
  allFiles<-allFiles[allFiles!="Mutation for every amplicons.xls"]
  if(funInd==1){
    for(i in 1:length(allFiles)){
      if(length(strsplit(allFiles[i],split="-")[[1]])!=6){
        stop(paste(allFiles[i],"is not properly named"))
      }else if(strsplit(allFiles[i],split="-")[[1]][1]!="PAT"){
        stop(paste(allFiles[i],"does not start with PAT-"))
      }else if(substr(strsplit(allFiles[i],split="-")[[1]][2],1,2)!="ID"){
        stop(paste(allFiles[i],"does not have proper patient ID"))
      }else if(strsplit(allFiles[i],split="-")[[1]][3]!="DNA"){
        stop(paste(allFiles[i],"does not have proper DNA"))
      }else if(substr(strsplit(allFiles[i],split="-")[[1]][4],1,2)!="ID"){
        stop(paste(allFiles[i],"DNA ID is missing or improperly named"))
      }else if(!strsplit(allFiles[i],split="-")[[1]][5]%in%c("OP","Curettage","NORMAL")){
        stop(paste(allFiles[i],"does not have propery label OP, Curettage, or NORMAL"))
      }
    }
    message("All file names ok for fluidigmValidationMain")
  }else if(funInd==2){
    allFiles<-list.files(dirFiles)
    if(!("Mutation for every amplicons.xls"%in%allFiles)){
      stop("Mutation for every amplicons.xls does not exist or is improperly named")
    }
    allFiles<-allFiles[allFiles!="Mutation for every amplicons.xls"]
    for(i in 1:length(allFiles)){
      if(length(strsplit(allFiles[i],split="-")[[1]])!=5){
        stop(paste(allFiles[i],"is not properly named"))
      }else if(strsplit(allFiles[i],split="-")[[1]][1]!="PAT"){
        stop(paste(allFiles[i],"does not start with PAT-"))
      }else if(substr(strsplit(allFiles[i],split="-")[[1]][2],1,2)!="ID"){
        stop(paste(allFiles[i],"does not have proper patient ID"))
      }else if(strsplit(allFiles[i],split="-")[[1]][3]!="DNA"){
        stop(paste(allFiles[i],"does not have proper DNA"))
      }else if(substr(strsplit(allFiles[i],split="-")[[1]][4],1,2)!="ID"){
        stop(paste(allFiles[i],"DNA ID is missing or improperly named"))
      }
    }
  }
}
