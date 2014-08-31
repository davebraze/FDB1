##' Coerce a vector of class "factor" to numeric
##'
##' Coerces a vector of class factor to numeric. This function preserves underlying integer level
##' associated with each element of f.
##' @title Coerce a vector of class "factor" to numeric
##' @param f : a vector of class "factor"
##' @return a vector of class "numeric"
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @seealso \code{\link{fact2char}}
fact2num <- function(f) {
    as.numeric(f)
}

##' Coerce a vector of class "factor" to character
##'
##' Coerces a vector of class factor to numeric. This function preserves level labels associated
##' with each element of f.
##' @title Coerce a vector of class "factor" to character
##' @param f : a vector of class factor
##' @return a vector of class "character"
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @seealso \code{\link{fact2num}}
fact2char <- function(f) {
    as.character(f)
}

