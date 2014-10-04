##' Coerces a vector of class factor to numeric. This function preserves underlying integer level
##' associated with each element of f.
##' @title Coerce a vector of class "factor" to numeric.
##' @param f A vector of class "factor".
##' @return A vector of class "numeric".
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @seealso \code{\link{fact2char}}
##' @export
fact2num <- function(f) {
    as.numeric(f)
}

##' Coerce a vector of class "factor" to character
##'
##' Coerces a vector of class factor to numeric. This function preserves level labels associated
##' with each element of f.
##' @title Coerce a vector of class "factor" to character
##' @param f A vector of class factor.
##' @return A vector of class "character".
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @seealso \code{\link{fact2num}}
##' @export
fact2char <- function(f) {
    as.character(f)
}

