##' Coerce a vector of class "factor" to numeric
##'
##' Coerces a vector of class factor to numeric. This function preserves underlying integer level
##' associated with each element of f.
##' @title Coerce a vector of class "factor" to numeric
##' @param f a vector of class "factor"
##' @return a vector of class "numeric"
##' @seealso \code{\link{fact2char}}
##' @author Dave Braze
fact2num <- function(f) {
    as.numeric(f)
}

##' Coerce a vector of class "factor" to character
##'
##' Coerces a vector of class factor to numeric. This function preserves level labels associated
##' with each element of f.
##' @title Coerce a vector of class "factor" to character
##' @param f a vector of class factor
##' @return a vector of class "character"
##' @seealso \code{\link{fact2num}}
##' ##' @author Dave Braze
fact2char <- function(f) {
    as.character(f)
}

