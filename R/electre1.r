#' Electre I 
#' 
#' ELECTRE I. is multicriteria decision analysis method from family of ELECTRE methods.
#' @param matrix data.frame (n \times m) of n alternatives and m crierias
#' @param weights data.frame of weights size 1 \times n 
#' @param c 
#' @param d 
#' @import dplyr
#' @export electre1
#' @examples 
#' matrix <- structure(list(dist = c(6, 3, 10), 
#'                          food = c(10, 5, 3), atm = c(4, 4, 5),
#'                          serv = c(3, 4, 1)),
#'                          .Names = c("dist", "food", "atm","serv"), 
#'                     row.names = c(NA, -3L), class = "data.frame")
#' weights <- structure(list(X1 = c(0.1, 0.45, 0.25, 0.2)), 
#'                      .Names = "X1", row.names = c(NA, -4L), class = "data.frame")
#' 
#' electre1(matrix, weights, c=0.5, d=0.9)

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
