##' Number of non-missing values.
##'
##' Returns the number of non-missing values in vector x. A convenience wrapper around sum(!is.na(x)).
##' @param v : a vector
##' @return integer, length of x minus the number of NAs in x.
##' @author David Braze \email{davebraze@@gmail.com}
##' @export
nobs <-
    function (v){
        if (!is.vector(v)) {
            warning('v must be a vector')
            return()
        }
        sum(!is.na(v))
    }
