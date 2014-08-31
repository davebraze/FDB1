##' Returns TRUE where n is odd.
##'
##' @title Returns TRUE where n is odd.
##' @param n : a numeric vector
##' @return TRUE if n is an integer and is odd, FALSE if n is an integer and not odd, NA otherwise.
##' @author David Braze \email{davebraze@@gmail.com}
##' @export
is.odd <-
    function(n) {
        if(n%%1){
            NA
        } else {
            if(n%%2){
                TRUE
            } else {
                FALSE
            }
        }
    }
