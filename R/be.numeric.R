##' Checks whether each element in a vector of strings can be interpreted as a number.
##'
##' Checks whether a (vector of) strings can be interpreted as a number.
##' @title Checks whether each element in a vector of strings can be interpreted as a number.
##' @param c a vector of strings
##' @return a vector of bool
##' @author cribbed from \url{http://rosettacode.org/wiki/Determine_if_a_string_is_numeric#R}
##' @export
be.numeric <- function(c) {
    suppressWarnings(!is.na(as.numeric(c)))
}
