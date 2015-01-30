##' Checks whether each element in a vector of strings can be coerced to numeric.
##' @title Checks element-wise if character vector can be coerced to numeric.
##' @param c : a vector of strings
##' @return a vector of logicals
##' @author cribbed from \url{http://rosettacode.org/wiki/Determine_if_a_string_is_numeric#R}
##' @export
is.numeral <- function(c) {
    suppressWarnings(!is.na(as.numeric(c)))
}
