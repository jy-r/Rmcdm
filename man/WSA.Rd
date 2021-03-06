% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WSA.r
\name{WSA}
\alias{WSA}
\title{WSA}
\usage{
WSA(matrix, weights, which_is_min)
}
\arguments{
\item{matrix}{data.frame (n \times m) of n alternatives and m crierias}

\item{weights}{data.frame of weights size 1 \times n}

\item{which_is_min}{logical vector for which criterias should be minimalized}
}
\description{
wsa mcdm method
}
\examples{
matrix <- structure(list(edu = c(4, 2, 3, 4, 4, 3), eng = c(1, 2, 2, 3, 2, 3), 
                         pc = c(5, 4, 5, 2, 2, 3), prax = c(1, 0, 3, 2, 1, 3), 
                         int = c(85, 80, 85, 70, 65, 90)), .Names = c("edu", "eng","pc", "prax", "int"),
                          row.names = c(NA, -6L), class = "data.frame")
weights <- structure(list(X1 = c(0.2, 0.333333333333333, 0.133333333333333, 
                                0.0666666666666667, 0.266666666666667)),
                                 .Names = "X1", row.names = c(NA, -5L), class = "data.frame")
which_is_min = rep(0,5)
WSA(matrix, weights, which_is_min)
}
