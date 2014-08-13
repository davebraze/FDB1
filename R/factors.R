##' Coerce a factor to numeric
##'
##' Preserves underlying integer level associated with each element ....
##' @title FDB1::fact2num()
##' @param f, a factor
##' @return a vector of class "numeric"
##' @author Dave Braze
fact2num <- function(f) {
    as.numeric(f)
}

##' Coerce a factor to character
##'
##' Preserves level labels associated with each element ....
##' @title fact2char()
##' @param f, a factor
##' @return a vector of class "character"
##' @author Dave Braze
fact2char <- function(f) {
    as.character(f)
}

