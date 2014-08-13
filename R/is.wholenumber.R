##' Can x reasonably be interpreted as a whole number?
##'
##' Takes a numeric vector and returns a Logical vector the same length
##' @title FDB1::is.wholenumber()
##' @param v: a numeric vector
##' @param tol:
##' @return a vector of Logicals
##' @author Cribbed from R example for base::integer().
##' @export
is.wholenumber <- function(v, tol = .Machine$double.eps^0.5) {
    abs(v - round(v)) < tol
}
