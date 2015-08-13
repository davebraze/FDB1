##' @title Locate outliers.
##' Given a numerical vector, locate values of \code{cut} or more standard deviations from the mean.
##'
##' @param x A numerical vector.
##' @param cut The cutpoint in sd units; defaults to 2.5.
##' @return A logical vector the length of \code{x}, TRUE wherever \code{x} is more extreme than \code{cut}.
##' @author David Braze \email{davebraze@@gmail.com}
##' @export
outlierID <- function(x, cut=2.5) {
    xx <- scale(x)
    retval <- ifelse (abs(xx) >= cut, TRUE, FALSE)
    retval
}

##' @title Replace outliers with NA.
##' Given a numerical vector, replace values of \code{cut} or more standard deviations from the mean with NAs.
##'
##' @param x A numerical vector.
##' @param cut The cutpoint in sd units; defaults to 2.5.
##' @return A numerical vector, a copy of \code{x} with outliers replace by NAs.
##' @author David Braze \email{davebraze@@gmail.com}
##' @export
outlierTrim <- function(x, cut=2.5) {
    id <- outlierID(x, cut=cut)
    retval <- ifelse(id, NA, x)
}

##' @title Outlier summary.
##' Given a numerical vector, indicate number of outliers, and numbers in lower and upper tails.
##'
##' @param x A numerical vector.
##' @param cut The cutpoint in sd units; defaults to 2.5.
##' @return A 3-vector with named elements "total", "lower", and "upper" with values indicating the
##' total number of outliers, outliers in the lower tail and outliers in the upper tail.
##' @author David Braze \email{davebraze@@gmail.com}
##' @export
outlierCheck <- function(x, cut=2.5) {
    id <- outlierID(x, cut=cut)
    mn <- mean(x, na.rm=T)
    o <- x[id]
    t <- length(o)
    l <- length(o[o<mn])
    u <- length(o[o>mn])
    retval <- c(total=t, lower=l, upper=u)
    retval
}
