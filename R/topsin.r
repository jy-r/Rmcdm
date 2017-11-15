#' Topsin 
#' 
#' electre mcdm method
#' @param matrix matrix (n \times m) of n alternatives and m crierias
#' @param weights vector of weights size n 
#' @import dplyr
#' @export topsin
#' @examples 


topsin <- function(matrix, weights) {
  weights <- data.frame(weights)
  ele <- data.frame(matrix)
  n = nrow(matrix)
  
  
  norm <- sqrt(colSums(top^2)) 
  norm <- matrix(unlist(rep(norm,n)), ncol=ncol(matrix), byrow=TRUE)
  
  weights.top <- matrix(unlist(rep(weights,n)), ncol=ncol(matrix), byrow=TRUE)
  
  
  R = top/norm
  
  Rw = R*weights.top
  
  ideal <- Rw %>% summarise_all(max)
  ideal <- matrix(unlist(rep(ideal,n)), ncol=ncol(matrix), byrow=TRUE)
  basal <- Rw %>% summarise_all(min)
  basal <- matrix(unlist(rep(basal,n)), ncol=ncol(matrix), byrow=TRUE)
  
  dplus <- sqrt(rowSums((Rw-ideal)^2))
  
  dminus  <- sqrt(rowSums((Rw-basal)^2))
  
  ci = dminus/(dplus+dminus)
  
  result <-
    data.frame(ci = ci,
               rank = order(ci, decreasing = TRUE))
  return(result)
}