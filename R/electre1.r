#' Electre I 
#' 
#' electre mcdm method
#' @param matrix data.frame (n \times m) of n alternatives and m crierias
#' @param weights data.frame of weights size 1 \times n 
#' @param c 
#' @param d 
#' @import dplyr
#' @export electre1
#' @examples 


electre1 <- function(matrix, weights, c, d) {
  ele <- matrix
  
  sij <- matrix(rep(0,nrow(matrix)^2),nrow=nrow(matrix))


  for(i in 1:nrow(matrix)){
    for(j in 1:nrow(matrix)){
      if(i==j){sij[i,j]=0}else{
        sij[i,j] <- sum(weights[ele[i,]>ele[j,]])
      }
    }

  }
  
  sieqj <- matrix(rep(0,nrow(matrix)^2),nrow=nrow(matrix))
  

  
  for(i in 1:nrow(matrix)){
    for(j in 1:nrow(matrix)){
      if(i==j){sieqj[i,j]=0}else{
        sieqj[i,j] <- sum(weights[ele[i,]==ele[j,]])
      }
    }

  }
  
  cij = sij + sieqj 
  
  
  dij <- matrix(rep(0,nrow(matrix)^2),nrow=nrow(matrix))
  

  for(i in 1:nrow(matrix)){
    for(j in 1:nrow(matrix)){
      y <- ele[i,]<ele[j,]
      if(any(y)){
        dij[i,j] <- max(abs(ele[i,y]-ele[j,y]))/max(abs(ele[i,]-ele[j,]))
      }
    }
  }
  
  dij[is.infinite(dij)]<-0
  
  
  pij <- matrix(as.numeric(cij>c & dij<d),nrow=nrow(matrix))
  
  pij <- as.data.frame(pij)
  result = rowSums(pij)
  return(list(result=result, pref_matrix=pij) )
}
