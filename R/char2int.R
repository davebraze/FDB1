##' @details Maps a character vector to an integer vector. Strings are mapped to integers in the
##' order that they are encountered, as enumerated from the beginning of the input vector. All
##' occurances of a given string are mapped to the same integer. Integer enumeration spans
##' \code{1:n}, where \code{n == length(unique(v))}.
##'
##' R> v1
##'  [1] "f" "f" "f" "a" "a" "a" "a" "f" "f" "d" "d" "d" "d" "d" "a" "a"
##' R> char2int(v1)
##' f f f a a a a f f d d d d d a a
##' 1 1 1 2 2 2 2 1 1 3 3 3 3 3 2 2
##'
##' @title Map character elements to integer elements.
##' @param v A character vector.
##' @return A vector of class integer.
##' @author
##' David Braze \email{davebraze@@gmail.com}
##' @seealso \code{\link{series}}
##' @export
char2int <- function(v) {
    v.labels <- unique(v)
    retval <- unlist(Map(function(e) which(v.labels==e), v))
    retval
}

if(FALSE) {
    v1 <- c(rep("f", 3), rep("a", 4), rep("f", 2), rep("d", 5), rep("a", 2))
    o1 <- char2int(v1)
    series(o1, step=0)

    v2 <- c(rep("mary", 3), rep("fred", 4), rep("mary", 2), rep("dan", 5), rep("fred", 2))
    o2 <- char2int(v2)
    series(o2, step=0)
}



