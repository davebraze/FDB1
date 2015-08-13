##' Checks whether each element in a vector of strings can be coerced to numeric.
##' @title Checks element-wise if character vector can be coerced to numeric.
##' @param c : a vector of strings
##' @return a vector of logicals, NA where is.na(c)
##' @author Basics cribbed from \url{http://rosettacode.org/wiki/Determine_if_a_string_is_numeric#R}
##' @author David Braze \email{davebraze@@gmail.com}
##' @export
is.numeral <- function(c) {
    retval <- suppressWarnings(!is.na(as.numeric(c)))
    retval[is.na(c)] <- NA
    retval
}

if(FALSE){
    x <- c('a', 'IX', 'twelve', '0001', '2', '3e1', '-4', '5.5', 'Inf', NA)
    data.frame(x,
               is.numeral(x),
               as.numeric(x))
}

