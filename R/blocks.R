##' @include is.wholenumber.R

##' @details Takes a vector of class character and returns a data.frame with 3 columns: (1) character string contained within block,
##' (2) index pointing to beginning of block, (3) length of block
##' @title Find contiguous blocks of identical integers in a numeric vector.
##' @param v A character vector.
##' @param min.block The minimum block length.
##' @return A data.frame with 3 columns and 1 row for each block.
##' @author
##' David Braze \email{davebraze@@gmail.com}
##' @seealso \code{\link{series}}
##' @export
find.block <- function(v, min.block=2) {
    ## To be done.

}

if(FALSE) {
    v1 <- c(rep("f", 3), rep("a", 4), rep("f", 2), rep("d", 5), rep("a", 2))
    v1.labels <- unique(v1)
    v1.nlabels <- length(v1.labels)


    v2 <- c(rep("mary", 3), rep("fred", 4), rep("mary", 2), rep("dan", 5), rep("fred", 2))
    v2.labels <- unique(v2)
    v2.nlabels <- length(v2.labels)

}



