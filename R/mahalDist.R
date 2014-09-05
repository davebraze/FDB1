## library(car)

##' Get squared Mahalanobis distances for N observations in M dimensions. This is a convenience wrapper
##' around \code{\link[stats]{mahalanobis}}, which see. Variables are optionally scaled before
##' distances are computed. Incomplete observations will return NA.
##' @title Get squared Mahalanobis distances for N observations in M dimensions.
##' @param m : A data.frame or matrix. Observations in rows.
##' @param scale : If TRUE, scale variables before getting mahalanobis distances.
##' @param use : Observations to use in computing covariance matrix. Gets passed to \code{\link[stats]{cov}}.
##' @param center : Type of univariate center for each variable in \code{m}; "mean" or "median"
##' @return : A vector of squared Mahalanobis distances. Incomplete observations return NA.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @seealso \code{\link[stats]{mahalanobis}}
##' @seealso \code{\link[stats]{cov}}
mahalDist <- function(m, scale=TRUE, use="complete.obs", center="mean") {
    stopifnot(length(dim(m))==2,
              (center=="mean"|center=="median"))
    m <- as.matrix(m)
    if (scale) m <- apply(m,2,scale)
    if ("mean"==center) c <- apply(m, 2, mean, na.rm=TRUE)
    if ("median"==center) c <- apply(m, 2, median, na.rm=TRUE)
    S <- cov(m, use=use)

    retval <- mahalanobis(m,c,S)
    retval
}

##' QQplot of squared mahalanobis distance against Chi-square distribution.
##' @title QQplot of squared mahalanobis distance against Chi-square distribution.
##' @param D2 Vector of squared mahalanobis distances as calculated, for example, by \code{\link{mahalDist}}.
##' @param dim Number of variables (dimensions) entering into D2.
##' @param envp Width of confidence envelope passed to \code{\link[car]{qq.plot}}. Defaults to .95.
##' @param point.col Point color for plotting passed to \code{\link[car]{qq.plot}}. Defaults to "blue".
##' @return Used for the side effect of creating a plot.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @seealso \code{\link[stats]{mahalanobis}}
##' @seealso \code{\link{mahalDist}}
##' @seealso \code{\link[car]{qq.plot}}
mahalPlot <- function(D2, dim, envp=.95, point.col="blue") {
    qq.plot(D2, line="robust",
            distribution="chisq", df=dim,
            envelope=envp,
            ## labels=TRUE,
            xlab=bquote('Quantiles of ' * chi[.(dim)]^2),
            ylab=expression(D^2),
            col="blue",
            main = bquote("Q-Q plot of Mahalanobis" * ~D^2 *
                " vs. quantiles of" * ~ chi[.(dim)]^2))
    abline(h=c(2,10), v=c(2,10), lty=3, col="grey")
    ds <- paste(names(X1))
    ypos=15
    text(x=2.5, y=ypos, adj=c(0,0), bquote(.(dim) * "D space:"))
    for (ii in 1:length(ds))
        text(x=2.5, y=ypos-.5*ii, adj=c(0,0), bquote(~~~~~ .(ds[ii])))
    text(x=5, y=2.5, col=point.col, adj=c(0,0), bquote("Pointwise confidence envelope, p=" * .(envp)))
}

