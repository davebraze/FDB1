##' Extract betas from an lm object.
##'
##' There is no built in method for extracting standardized regression coefficients
##' (betas) from an \code{\link[stats]{lm}} object. This function fills that gap.
##'
##' @title Extract betas from an lm object.
##' @param lm : an lm object
##' @return A vector of standardized coefficients.
##' @author David Braze \email{davebraze@@gmail.com}
##' @seealso  \code{\link[stats]{lm}}
##' @export
betas = function(lm) {
  if(!(class(lm) %in% 'lm'))
    stop("Argument must be of class 'lm'")
  sds = sapply(lm$model, sd)
  (sds[-1]/sds[1])*coef(lm)[-1]
}

