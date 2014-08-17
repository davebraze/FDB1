##' Can each element of numeric vector x reasonably be interpreted as a whole number?
##'
##' Takes a numeric vector and returns a Logical vector the same length
##' @title Can each element of numeric vector x reasonably be interpreted as a whole number?
##' @param v: a numeric vector
##' @param tol: what counts as close enough to 0? Defaults to .Machine$double.eps^0.5
##' @return a vector of Logicals
##' @author Cribbed from R example for base::integer().
##' @export
is.wholenumber <- function(v, tol = .Machine$double.eps^0.5) {
    abs(v - round(v)) < tol
}
