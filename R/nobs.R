##' A convenience wrapper around sum(!is.na(x)).
##'
##' returns the number of non-missing values in vector x.
##' @param v a vector
##' @return integer, length of x minus the number of NAs in x.
##' @author David Braze
##' @export
nobs <-
    function (v){
        if (!is.vector(v)) {
            warning('v must be a vector')
            return()
        }
        sum(!is.na(v))
    }
