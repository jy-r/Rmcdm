% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/electre1.r
\name{electre1}
\alias{electre1}
\title{Electre I}
\usage{
electre1(matrix, weights, c, d)
}
\arguments{
\item{matrix}{data.frame (n \times m) of n alternatives and m crierias}

\item{weights}{data.frame of weights size 1 \times n}

\item{c}{}

\item{d}{}
}
\description{
ELECTRE I. is multicriteria decision analysis method from family of ELECTRE methods.
}
\examples{
matrix <- structure(list(dist = c(6, 3, 10), 
                         food = c(10, 5, 3), atm = c(4, 4, 5),
                         serv = c(3, 4, 1)),
                         .Names = c("dist", "food", "atm","serv"), 
                    row.names = c(NA, -3L), class = "data.frame")
weights <- structure(list(X1 = c(0.1, 0.45, 0.25, 0.2)), 
                     .Names = "X1", row.names = c(NA, -4L), class = "data.frame")

electre1(matrix, weights, c=0.5, d=0.9)
}
