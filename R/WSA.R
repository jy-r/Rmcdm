#' WSA
#' 
#' wsa mcdm method
#' @param matrix data.frame (n \times m) of n alternatives and m crierias
#' @param weights data.frame of weights size 1 \times n 
#' @param which_is_min logical vector for which criterias should be minimalized
#' @import dplyr
#' @export WSA
#' @examples 


WSA <- function(matrix, weights, which_is_min) {
  wsa <- matrix
  n = nrow(matrix)
    
    
  vmax <- wsa %>% summarise_all(max)
  vmax <- matrix(unlist(rep(vmax, n)), ncol = ncol(matrix), byrow = TRUE)
  
  vmin <- wsa %>% summarise_all(min)
  vmin <- matrix(unlist(rep(vmin, n)), ncol = ncol(matrix), byrow = TRUE)
  
  weights.wsa <-
    matrix(unlist(rep(weights, n)), ncol = ncol(matrix), byrow = TRUE)
  
  
  wsa <- as.matrix(wsa)
  
  wsa[, which_is_min] <-
    (vmax[, which_is_min] - wsa[, which_is_min]) / (vmax[, which_is_min] - vmin[, which_is_min])
  wsa[, !which_is_min] <-
    (wsa[, !which_is_min] - vmin[, !which_is_min]) / (vmax[, !which_is_min] - vmin[, !which_is_min])
  
  wsa <- wsa * weights.wsa
  
  wsa.sum <- rowSums(wsa)
  
  wsa <- as.data.frame(wsa)
  

  
  result <-
    data.frame(wsa = wsa.sum,
               rank = rank(-wsa.sum))
  return(result)
}
