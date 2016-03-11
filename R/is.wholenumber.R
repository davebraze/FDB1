##' @title Can each element of x reasonably be interpreted as a whole number?
##'
##' @description Determine whether each element of x reasonably be interpreted as a whole number.
##'
##' @details
##' Takes a numeric vector and returns a Logical vector the same length.
##'
##' @param v A numeric vector.
##' @param tol What counts as close enough to 0? Defaults to .Machine$double.eps^0.5.
##' @return A vector of Logicals.
##' @author Cribbed from example in \code{\link[base]{integer}}
##' @export
is.wholenumber <- function(v, tol = .Machine$double.eps^0.5) {
    abs(v - round(v)) < tol
}
