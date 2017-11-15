#' Electre I 
#' 
#' electre mcdm method
#' @param matrix matrix (n \times m) of n alternatives and m crierias
#' @param weights vector of weights size n 
#' @param c 
#' @param d 
#' @import dplyr, plyr
#' @export electre1
#' @examples 


electre1 <- function(matrix, weights) {
  weights <- data.frame(weights)
  ele <- data.frame(matrix)
  
  sij <- matrix(rep(0,nrow(dta)^2),nrow=nrow(dta))
  
  i = 1
  j = 1
  progress.bar <- create_progress_bar("text")
  progress.bar$init(nrow(dta))
  
  
  for(i in 1:nrow(dta)){
    for(j in 1:nrow(dta)){
      if(i==j){sij[i,j]=0}else{
        sij[i,j] <- sum(weights[ele[i,]>ele[j,]])
      }
    }
    #progress bar
    progress.bar$step()
  }
  
  sieqj <- matrix(rep(0,nrow(dta)^2),nrow=nrow(dta))
  
  progress.bar <- create_progress_bar("text")
  progress.bar$init(nrow(dta))
  
  for(i in 1:nrow(dta)){
    for(j in 1:nrow(dta)){
      if(i==j){sieqj[i,j]=0}else{
        sieqj[i,j] <- sum(weights[ele[i,]==ele[j,]])
      }
    }
    #progress bar
    progress.bar$step()
  }
  
  cij = sij + sieqj 
  
  
  dij <- matrix(rep(0,nrow(dta)^2),nrow=nrow(dta))
  
  progress.bar <- create_progress_bar("text")
  progress.bar$init(nrow(dta))
  for(i in 1:nrow(dta)){
    for(j in 1:nrow(dta)){
      y <- ele[i,]<ele[j,]
      if(any(y)){
        dij[i,j] <- max(abs(ele[i,y]-ele[j,y]))/max(abs(ele[i,]-ele[j,]))
      }
    }
    #progress bar
    progress.bar$step()
  }
  
  dij[is.infinite(dij)]<-0
  
  c = 0.5
  d = 0.9
  
  pij <- matrix(as.numeric(cij>c & dij<d),nrow=nrow(dta))
  
  pij <- as.data.frame(pij)
  result = rowSums(pij)
  return(result)
}