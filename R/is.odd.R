##' @title Is n even?
##'
##' @details
##' Test whether elements of a numeric vector are even. For whole numbers return TRUE or FALSE. For decimals return NA.
##'
##' @param n A numeric vector.
##' @return A logical vector the same length as n. TRUE if n is an integer and is even, FALSE if n
##' is an integer and not even, NA otherwise.
##' @author David Braze \email{davebraze@@gmail.com}
##' @export
is.even <- function (n)
{
    n[n %% 1 != 0] <- NA
    retval <- n %% 2 ==0
    retval
}

if(FALSE) {
    n <- 1:10
    is.even(n)

    m <- seq(1, 10, .5)
    is.even(m)
}

##' @title Is n odd?
##'
##' @details
##' Test whether elements of numeric vector are odd. For whole numbers return TRUE or FALSE. For decimals return NA.
##'
##' @param n A numeric vector.
##' @return A logical vector the same length as n. TRUE if n is an integer and is even, FALSE if n
##' is an integer and not even, NA otherwise.
##' @author David Braze \email{davebraze@@gmail.com}
##' @export
is.odd <-
    function(n) {
        !is.even(n)
    }
