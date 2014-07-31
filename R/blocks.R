##' @include is.wholenumber.R
##' Find contiguous blocks of identical integers in a numeric vector.
##'
##' Takes a vector of whole numbers and returns a matrix with 3 columns: (1) integer contained within block,
##' (2) index pointing to beginning of block, (3) length of block
##' @title FDB1::blocks()
##' @param v: a numeric vector containing only whole numbers
##' @return numeric matrix with 3 columns and 1 row for each block.
##' @author David Braze
##' @export
blocks <- function(v) {
    ## add functionality for character and factor vectors as well:
    ## cv <- rep(letters[1:3],each=3); as.integer(as.factor(cv))
    if ( any( is.na(v) )) {
        warning("Missing values removed from v.")
        v <- v[!is.na(v)]
    }
    if (!is.numeric(v)) stop("v must be numeric.")
    if (!all(is.wholenumber(v))) stop("v must be a vector of whole numbers.")
    runs <- c(-1, diff(v)!=0, NA)
    runlocs <- which(runs!=0)
    int <- v[runlocs]
    runlens <- diff(c(runlocs, length(v)+1))
    cbind(int, runlocs, runlens)
}

if (FALSE) {
    v1 <- c(rep(1,10), rep(5,9), rep(6,11))
    v2 <- rep(c(2,1,2,2,1), each=3)
    v3 <- rep(c(1,3,1,1,3), each=3)
    blocks(v1)
    blocks(v2)
    blocks(v3)
}
