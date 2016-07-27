#' Title function to calculate interaction between two groups (SS_AxB) in a two way annova test
#'
#' @param ms : list for all matrices
#'
#' @return SS_AxB
#' @export
#'
#' @examples
SS_AxB<-function(ms){
  K<-length(ms)
  I<-nrow(ms[[1]])
  J<-ncol(ms[[2]])
  total<-matrix(rep(0,I*J),nrow=I)
  val<-0
  for(k in 1:K){
    total<-total+(ms[[k]])
  }
  total_bar<-sum(total)/(I*J*K)
  for(i in 1:I){
    for(j in 1:J){
      y_i_bar<-0
      y_j_bar<-0
      y_ij_bar<-0
      for(jj in 1:J){
        for(kk in 1:K){
          y_i_bar<-y_i_bar+ms[[kk]][i,jj]
        }
      }
      for(ii in 1:I){
        for(kk in 1:K){
          y_j_bar<-y_j_bar+ms[[kk]][ii,j]
        }
      }
      for(kk in 1:K){
        y_ij_bar<-y_ij_bar+ms[[kk]][i,j]
      }
      y_i_bar<-y_i_bar/(J*K)
      y_j_bar<-y_j_bar/(I*K)
      y_ij_bar<-y_ij_bar/K
      val<-val+(y_ij_bar-y_i_bar-y_j_bar+total_bar)^2
    }
  }
  return(val*K)
}
