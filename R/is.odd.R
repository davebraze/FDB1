##' Returns TRUE if n is odd.
##'
##' @title FDB1::is.odd()
##' @param v a numeric vector
##' @return TRUE if n is an integer and is odd, FALSE if n is an integer and not odd, NA otherwise.
##' @author David Braze
##' @export
is.odd <-
    function(v) {
        if(n%%1){
            NA
        } else {
            if(v%%2){
                TRUE
            } else {
                FALSE
            }
        }
    }
