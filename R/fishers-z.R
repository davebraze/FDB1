##' Fishers Z for comparing two correllation coefficients.
##' Method cribbed from:
##'   http://www.fon.hum.uva.nl/Service/Statistics/Two_Correlations.html
##'   http://ftp.sas.com/techsup/download/stat/compcorr.html
##' and implemented in R:
##'
##' @title compcorr()
##' @param n1 n for correlation 1
##' @param r1 r for correlation 1
##' @param n2 n for correlation 2
##' @param r2 r for correlation 2
##' @return list containing diff(Z1, Z2) and pval for difference.
##' @author Dave Braze
##' @export
compcorr <- function(n1, r1, n2, r2){
# Fisher Z-transform
        zf1 <- 0.5*log((1 + r1)/(1 - r1))
        zf2 <- 0.5*log((1 + r2)/(1 - r2))
# difference
        dz <- (zf1 - zf2)/sqrt(1/(n1 - 3) + (1/(n2 - 3)))
 # p-value
        pv <- 2*(1 - pnorm(abs(dz)))

        return(list(diff=dz, pval=pv))
}
