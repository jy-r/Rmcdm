#' Topsis
#' 
#' Topsis Technique for Order of Preference by Similarity to Ideal Solution (TOPSIS)
#'  is a multi-criteria decision analysis method.
#' @param matrix data.frame (n \times m) of n alternatives and m crierias
#' @param weights data.frame of weights size 1 \times n 
#' @import dplyr
#' @export topsis
#' @examples 
#' matrix <- structure(list(price = c(240, 160, 200, 160, 50, 0, 220),
#'                          power = c(900,300, 100, 100, 100, 0, 100), 
#'                          ser = c(1, 1, 0, 0, 1, 1, 1), 
#'                          pos = c(1, 1, 1, 0, 1, 0, 1), 
#'                          safty = c(1, 1, 1, 0, 1, 0, 1), 
#'                          app = c(5, 7, 8, 4, 8, 6, 7)),
#'                          .Names = c("price", "power", "ser", "pos",  "safty", "app"), 
#'                          row.names = c(NA, -7L), class = "data.frame")
#' 
#' weights <- structure(list(X1 = c(0.206896551724138, 0.172413793103448, 0.0689655172413793, 
#'                                  0.137931034482759, 0.137931034482759, 0.275862068965517)),
#'                      .Names = "X1", row.names = c(NA, -6L), class = "data.frame")
#'                      
#' topsin(matrix, weights)




topsis <- function(matrix, weights) {
  top <- matrix
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
               rank = rank(-ci))
  return(result)
}
