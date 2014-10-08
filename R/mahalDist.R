## library(car)

##' Get squared Mahalanobis distances for N observations in M dimensions. This is a convenience wrapper
##' around \code{\link[stats]{mahalanobis}}, which see. Variables are optionally scaled before
##' distances are computed. Incomplete observations will return NA.
##' @title Get squared Mahalanobis distances for N observations in M dimensions.
##' @param m A data.frame or matrix. Observations in rows.
##' @param scale If TRUE, scale variables before getting mahalanobis distances.
##' @param use Observations to use in computing covariance matrix. Gets passed to \code{\link[stats]{cov}}.
##' @param center Type of univariate center for each variable in \code{m}; "mean" or "median"
##' @return
##' A list with additional class "mahalDist" containing elements:
##' \enumerate{
##'     \item{D2: A vector of squared Mahalanobis distances for observations (rows) in \code{m}.
##'             Incomplete observations return NA.}
##'     \item{vars: A character vector with the column names from \code{m}.}
##'     \item{dim: The number of columns of \code{m}.}
##' }
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @seealso \code{\link[stats]{mahalanobis}}
##' @seealso \code{\link[stats]{cov}}
##' @export
mahalDist <- function(m, scale=TRUE, use="complete.obs", center="mean") {
    stopifnot(length(dim(m))==2,
              (center=="mean"|center=="median"))
    m <- as.matrix(m)
    if (scale) m <- apply(m,2,scale)
    if ("mean"==center) c <- apply(m, 2, mean, na.rm=TRUE)
    if ("median"==center) c <- apply(m, 2, median, na.rm=TRUE)
    S <- cov(m, use=use)

    D2 <- mahalanobis(m,c,S)
    vars <- colnames(m)
    dim <- dim(m)[2]
    retval <- list(D2=D2, vars=vars, dim=dim)
    class(retval) <- c("mahalDist", class(retval))
    retval
}

##' QQplot of squared mahalanobis distance against Chi-square distribution.
##' @title QQplot of squared mahalanobis distance against Chi-square distribution.
##' @param D2 Vector of squared mahalanobis distances as calculated, for example, by \code{\link{mahalDist}}.
##' @param dim Number of variables (dimensions) entering into D2.
##' @param envp Width of confidence envelope passed to \code{\link[car]{qqPlot}}. Defaults to .95.
##' @param point.col Color for plotting points passed to \code{\link[car]{qqPlot}}. Defaults to "blue".
##' @return Used for the side effect of creating a plot.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @seealso \code{\link[stats]{mahalanobis}}
##' @seealso \code{\link{mahalDist}}
##' @seealso \code{\link[car]{qqPlot}}
##' @export
mahalPlot <- function(X, envp=.95, col.point="blue", ...) {
    stopifnot("mahalDist" %in% class(X))
    car::qqPlot(X$D2, line="robust",
                distribution="chisq", df=X$dim,
                envelope=envp,
                xlab=bquote('Quantiles of ' * chi[.(X$dim)]^2),
                ylab=expression(D^2),
                col=col.point,
                main = bquote("Q-Q plot of Mahalanobis" * ~D^2 *
                    " vs. quantiles of" * ~ chi[.(X$dim)]^2),
                ...)
    abline(h=c(2,10), v=c(2,10), lty=3, col="grey")
    ypos=15
    text(x=2.5, y=ypos, adj=c(0,0), bquote(.(dim) * "D space:"))
    for (ii in 1:length(X$vars))
        text(x=2.5, y=ypos-.8*ii, adj=c(0,0), bquote(~~~~~ .(X$vars[ii])))
    text(x=5, y=2.5, col="black", adj=c(0,0), bquote("Pointwise confidence envelope, p=" * .(envp)))
}

