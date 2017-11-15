#' Topsin 
#' 
#' electre mcdm method
#' @param matrix matrix (n \times m) of n alternatives and m crierias
#' @param weights vector of weights size n 
#' @import dplyr
#' @export electre1
#' @examples 


topsin <- function(matrix, weights) {
  n = nrow(dta)
  
  #transform columns from min to max (in example skip)
  vmax <- top %>% summarise_all(max)
  top[,14]<-vmax[14]-top[,14]
  top[,15]<-vmax[15]-top[,15]
  top[,16]<-vmax[16]-top[,16]
  
  norm <- sqrt(colSums(top^2)) 
  norm <- matrix(unlist(rep(norm,n)), ncol=ncol(dta), byrow=TRUE)
  
  weights.top <- matrix(unlist(rep(weights,n)), ncol=ncol(dta), byrow=TRUE)
  
  
  R = top/norm
  
  Rw = R*weights.top
  
  ideal <- Rw %>% summarise_all(max)
  ideal <- matrix(unlist(rep(ideal,n)), ncol=ncol(dta), byrow=TRUE)
  basal <- Rw %>% summarise_all(min)
  basal <- matrix(unlist(rep(basal,n)), ncol=ncol(dta), byrow=TRUE)
  
  dplus <- sqrt(rowSums((Rw-ideal)^2))
  
  dminus  <- sqrt(rowSums((Rw-basal)^2))
  
  ci = dminus/(dplus+dminus)
  
  result <-
    data.frame(ci = ci,
               rank = order(ci, decreasing = TRUE))
  return(result)
}