##' @title Compute cosine distance.
##' Takes a numeric matrix (M) of arbitrary dimension, treats rows of M as vectors and computes the
##' pairwise cosine distances among the rows of M. Return a matrix of pairwise distances with vector
##' magnitudes on the diagonal.
##'
##' @param M A numeric matrix.
##' @return A matrix with pairwise cosine distances in the lower triangle and vector magnitudes on
##' the diagonal.
##' @author
##' Dave Braze \email{davebraze@@gmail.com}
##' @seealso
##' \code{\link{dist}}
##' @export
cosinedist <- function(M) {
  origin <- rep(0, dim(M)[2])
  # sum(Vn*Vm) # for each pair of vectors
  n <- tcrossprod(M,M)
  # get the length of each vector
  V.mag <- as.matrix(dist(rbind(origin, M)))[-1,1]
  d <- tcrossprod(V.mag)
  retval <- n/d
  retval[upper.tri(retval)] <- NA
  diag(retval) <- V.mag # vector magnitudes on the diagonal
  retval
}

if(FALSE){

  oo <- c(0,0,0)
  v1 <- c(0,1.7,1)
  v2 <- c(1.6,0,1)
  v3 <- c(1.8,1.8,1)
  v4 <- c(.5,1,1)
  v5 <- c(1.5,.7,1)
  v6 <- c(.33,1.5,1)
  M1 <- rbind(v1,v2,v3,v4,v5,v6)

  M1j <- jitter(cbind(M1, 0))

  library(gplots)
  col <- rich.colors(dim(M1)[1])
  plot(NULL, xlim=c(0,2), ylim=c(0,2), type='n', xlab="x", ylab="y")
  for (ii in 1:dim(M1)[1]) {
    lines(rbind(oo, M1[ii,]), col=col[ii])
  }
  legend(1.8,1.5, paste("v", as.character(1:dim(M1)[1]), sep=""),
         text.col=col)

  library(gplots)
  col <- rich.colors(dim(M1j)[1])
  plot(NULL, xlim=c(0,2), ylim=c(0,2), type='n', xlab="x", ylab="y")
  for (ii in 1:dim(M1j)[1]) {
    lines(rbind(oo, M1j[ii,]), col=col[ii])
  }
  legend(1.8,1.5, paste("v", as.character(1:dim(M1j)[1]), sep=""),
         text.col=col)


  M2 <- M1
  M2[,3] <- 2
  M3 <- M1
  M3[,3] <- 4
  M4 <- M1*-1
  M5 <- rbind(M1,M4)

  library(gplots)
  plot(NULL, xlim=c(-2,2), ylim=c(-2,2), type='n', xlab="x", ylab="y")
  col <- rich.colors(dim(M5)[1])
  for (ii in 1:dim(M5)[1]) {
    lines(rbind(oo, M5[ii,]), col=col[ii])
  }
  legend(-2,2, paste("v", as.character(1:dim(M5)[1]), sep=""),
         text.col=col)

}
