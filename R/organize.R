#' Title function to create a folder for each patient and move all files of that patient to that folder created
#'
#' @param dirFiles : directory of all original patient files
#'
#' @return
#' @export
#'
#' @examples
organize<-function(dirFiles){
  myFiles<-list.files()
  if(length(myFiles)<1){
    stop("No raw files. Check the files in the directory you entered")
  }
  allPatients<-NULL
  for(i in 1:length(myFiles)){
    allPatients<-append(allPatients,strsplit(myFiles[i],split="[-]")[[1]][2])
  }
  allPatients<-unique(allPatients)
  for(i in 1:length(allPatients)){
    dir.create(allPatients[i])
  }
  for(i in 1:length(myFiles)){
    file.copy(myFiles[i],paste(".\\",getPatientID(myFiles[i]),"\\",myFiles[i],sep=""))
    file.remove(myFiles[i])
  }
}
