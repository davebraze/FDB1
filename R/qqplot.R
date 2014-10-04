## library(ggplot2)
## library(gridExtra)

##' Given a data.frame and the name of a variable within it create a quantile-quantile plot based on
##' the normal distribution. Also plot a 'fit' line with intercept equal to sample mean and slope
##' equal to sample standard deviation.
##' @title Create a QQ plot for a variable within a data.frame.
##' @param v A character string naming a variable in data.frame 'dat'.
##' @param dat A data.frame containing at least 1 variable 'v'.
##' @return a \pkg{ggplot2} object
##' @seealso \code{\link{qqplot}}
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
qqp <- function(v, dat) {
    mn <- mean(dat[[v]], na.rm=T) # maybe add mean and sd to lower-right corner of plot?
    sd <- sd(dat[[v]], na.rm=T)
    p <- ggplot(dat, aes_string(sample=v))  # NOTE: use of aes_string() in place of aes()
    p <- p+geom_point(stat="qq")
    p <- p+geom_abline(intercept = mn, slope = sd,
                       size=1.25, col="blue", alpha=1/3)
    p <- p+coord_fixed(ratio=1/sd)  # TODO: better to do diff(range(sample))/(diff(range(sample)) / s)
    p <- p+ggtitle(v) # TODO: drop title and add label to upper-left corner of plot
    return(p)
}

##' Given a data.frame and vector of variable names within it, create a matrix of quantile-quantile
##' plots based on the normal distribution and include 'fit' lines on each with intercepts equal to
##' sample means and slopes equal to sample standard deviations.
##' @title Create and plot a QQ plot matrix.
##' @param vars A character vector specifying variables with data.frame 'dat' to plot.
##' @param dat A data.frame containing variables to be plotted.
##' @return A grid grob created by applying grid.arrange to list of ggplot2 objects.
##' @seealso \code{\link{qqp}}
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
qqplot <- function(vars, dat) {
    plist <- lapply(vars, FUN=qqp, dat=dat)
    do.call(grid.arrange, plist)
    ## qqpm <- do.call(grid.arrange, plist)
    ## return(qqpm)
}
