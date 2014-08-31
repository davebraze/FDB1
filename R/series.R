##' @include is.wholenumber.R
##' Find sequences of positive integers in vector v.
##'
##' @details
##' Takes a vector of whole numbers and returns a matrix with 3 columns: (1) integer at beginning of
##' series, (2) index pointing to beginning of series, (3) length of series
##' @title Find sequences of positive integers in numeric vector v.
##' @param v : a numeric vector.
##' @return numeric matrix with 3 columns and 1 row for each series.
##' @author
##' David Braze \email{davebraze@@gmail.com}
##' @seealso \code{\link{blocks}}
##' @export
series <- function(v){
    if (any(is.na(v))) {
        warning("Missing values removed from v.")
        v <- v[!is.na(v)]
    }
    if (!is.numeric(v)) stop("v must be numeric.")
    if (!all(sign(v)==1)) stop("v must contain only positive numbers.")
    if (!all(is.wholenumber(v))) stop("v must contain only whole numbers.")
    runs <- (c(-1, diff(v)!=1, NA))
    runlocs <- which(runs!=0)
    int <- v[runlocs]
    runlens <- diff(c(runlocs, length(v)+1))
    cbind(int, runlocs, runlens)
}

if (FALSE) {
    v4 <- c(2:5, 7:11, 13, 4:9, 11:13, 1:6, 101:105)
    v5 <- v2; v5[3] <- NA
    series(v4)
    series(-v4)
    series(v5)
}

