##' @title Plot histograms for two samples drawn from Normal distributions.
##'
##' @description Plot a pair of histograms.
##'
##' @details
##' Plot histograms of two samples drawn from different Normal distributions, given Mean, SD and N for each.
##'
##' @param mn1 Mean for sample 1
##' @param sd1 SD for sample 1
##' @param n1 N for sample 1
##' @param mn2 Mean for sample 2
##' @param sd2 SD for sample 2
##' @param n2 N for sample 2
##' @param binwidth binwidth for histogram
##' @param stack stack plots vertically or no; defaults to FALSE (side-by-side plots)
##' @param aspect set aspect ratio of plot; defaults to 2/5
##' @return used for side-effect of creating a plot
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
normalSampHist2 <- function(mn1, sd1, n1, mn2, sd2, n2, binwidth=mean(c(sd1,sd2))/2, stack=FALSE, aspect=2/5){
    a=rnorm(n1, mn1, sd1)
    b=rnorm(n2, mn2, sd2)
    id1 <- rep('a', length=length(a))
    id2 <- rep('b', length=length(b))
    idab <- c(id1,id2)
    ab <- c(a,b)
    df1 <- data.frame(idab, ab)
    p0 <- ggplot2::ggplot(data=df1, ggplot2::aes(x=ab))
    p0 <- p0 + ggplot2::geom_histogram(binwidth=binwidth, ggplot2::aes(fill=idab), color="black")
    if(!stack) p0 <- p0 + ggplot2::facet_wrap(~idab)
    else p0 <- p0 + ggplot2::facet_wrap(~idab,ncol=1)
    p0 <- p0 + ggplot2::xlab("Value/Score") + ggplot2::ylab("Count")
    p0 <- p0 + ggplot2::coord_fixed(ratio=aspect)
    p0
}

##' @title Plot histograms of two different theoretical normal distributions.
##'
##' @details
##' Plot histograms of two different theoretical normal distributions, given Mean, SD and N for each.
##'
##' ToDo:
##' \enumerate{
##'     \item print parameters on each facet. See largeNumbers() below.
##'     \item allow for overlaid histograms (single facet).
##'     \item add label (geom_text()) for parameters and N to each facet.
##'     \item add a vlines for means
##'     \item optionally draw rectangles around mean+-Xsd for each distribution
##'     \item optionally specify colors of histograms
##' }
##'
##' @param mn1 Mean for Distribution 1
##' @param sd1 SD for Distribution 1
##' @param n1 N for scaling Histogram 1 (convert density to count)
##' @param mn2 Mean for Distribution 1
##' @param sd2 SD for Distribution 1
##' @param n2 N for scaling Histogram 1 (convert density to count)
##' @param stack stack plots vertically or no; defaults to FALSE (side-by-side plots)
##' @param aspect aspect ratio for plots; defaults to 2/5
##' @return used for its side-effect of creating a plot
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
normalDistHist2 <- function(mn1, sd1, n1, mn2, sd2, n2, stack=FALSE, aspect=2/5){
    range1 <- c(mn1-sd1*3.5, mn1+sd1*3.5)
    range2 <- c(mn2-sd2*3.5, mn2+sd2*3.5)
    xmin <- min(min(range1), min(range2))
    xmax <- max(max(range1), max(range2))
    x <- xmin:xmax
    dnorm1 <- dnorm(x, mn1, sd1)
    bin1 <- round(dnorm1*n1) # convert dens. to 'count'
    dnorm2 <- dnorm(x, mn2, sd2)
    bin2 <- round(dnorm2*n2) # convert dens to 'count'
    id <- rep(c('X1', 'X2'), each=length(x))
    count <- c(bin1,bin2)
    score <- rep(x, 2)
    df2 <- data.frame(id, count, score)
    ## browser()
    p1 <- ggplot2::ggplot(data=df2, ggplot2::aes(y=count, x=score, fill=id))
    p1 <- p1 + ggplot2::geom_bar(stat='identity', width=.1)
    if(!stack) p1 <- p1 + ggplot2::facet_wrap(~id)
    else p1 <- p1 + ggplot2::facet_wrap(~id,ncol=1)
    ## Can't seem to set bar width. I think it has to do with the fact that binwidth is 1
    p1 <- p1 + ggplot2::xlab("Value/Score") + ggplot2::ylab("Count")
    p1 <- p1 + ggplot2::coord_fixed(ratio=aspect)
    p1
}

##' @title Histogram of gamma distribution.
##'
##' @details
##' Histogram of gamma distribution and its reverse.
##' TODO: use geom_histogram() instead of geom_bar() for consistency with other functions
##' TODO: label with (mean, median, mode)
##'
##' @param x 'x' vector argument to \code{\link[stats]{dgamma}}
##' @param df1 'shape' argument to \code{\link[stats]{dgamma}}
##' @param df2 'rate' argument to \code{\link[stats]{dgamma}}
##' @param mult multiplier used to convert density to counts
##' @param aspect aspect ratio of plots; defaults to 2/5
##' @param stack stack plots or no; defaults to FALSE (side-by-side plots)
##' @return Used for side effect of creating a plot.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
gammaDistHist <- function(x=seq(0,.7,by=.01), df1=6, df2=25, mult=20, aspect=2/5, stack=TRUE) {
    dgamma1 <- dgamma(x, df1, df2, log = FALSE)
    freq1 <- round(dgamma1*mult)
    dat1 <- unlist(mapply(rep, 1:length(freq1), freq1))
    print(mean(dat1))
    print(median(dat1))
    print(which.max(freq1))
    freq1r <- rev(freq1)
    bins <- 1:length(x); bins <- c(bins, bins)
    freq <- c(freq1, freq1r)
    dist <- rep(c('a', 'b'), each=length(x))
    df3 <- data.frame(freq, bins, dist)
    p1 <- ggplot2::ggplot(data=df3, ggplot2::aes(y=freq, x=bins, fill=dist))
    p1 <- p1 + ggplot2::geom_bar(stat='identity', width=.1, color='black')
    if(!stack) p1 <- p1 + ggplot2::facet_wrap(~dist)
    else p1 <- p1 + ggplot2::facet_wrap(~dist, ncol=1)
    p1 <- p1 + ggplot2::coord_fixed(ratio=aspect)
    p1
}

##' Two normal curves with (pontentially) different means and SDs.
##'
##' Plot two normal curves with (pontentially) different means and SDs.
##' @title Normal curve plot
##' @param mn1 Mean for curve 1
##' @param sd1 SD for curve 1
##' @param mn2 Mean for curve 2
##' @param sd2 SD for curve 2
##' @param expanse multiplier for scaling plot x dimension
##' @param xlim 'xlim' argument to plot()
##' @return used for side effect of creating a plot.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
normal2 <- function(mn1, sd1, mn2, sd2, expanse=3.5, xlim=NA){
    range1 <- c(mn1-sd1*expanse, mn1+sd1*expanse)
    range2 <- c(mn2-sd2*expanse, mn2+sd2*expanse)
    xmin <- min(min(range1), min(range2))
    xmax <- max(max(range1), max(range2))
    x <- xmin:xmax
    dnormal1 <- dnorm(x, mn1, sd1)
    dnormal2 <- dnorm(x, mn2, sd2)
    ymax <- max(c(dnormal1, dnormal2))
    plot(dnormal1~x, type='l', col='black', ylim=c(0, ymax), xlim=xlim,
         lty=1, lwd=3, yaxt='n', ylab='f', xlab='X', cex.axis=1.5, cex.lab=1.5)
    lines(dnormal2~x, col='black', lty=2, lwd=3)
}

##' Plot normal curve with middle region painted.
##'
##' Note that this handles plots as per function stdnormal1() as a special case.
##' @title Plot normal curve with middle region painted.
##' @param mn the mean
##' @param sd standard deviation
##' @param expanse distance from mean for upper and lower plot limits in sd units. Defaults to 3.5.
##' @param min lower bound of painted region
##' @param max upper bound of painted region
##' @param region.col painted region color
##' @param line.col line color
##' @param label label for painted region
##' @param lwd line width
##' @return Used for side effect.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
normalRegion <- function(mn=100, sd=10, expanse=3.5, min, max, region.col="grey", line.col="black", label=FALSE, lwd=3) {
    ## case 1: draw a curve without a painted region, set min=max.
    ## case 2: draw curve with painted mid region, set min>(mn-sd*expanse) and (max>min)&(max<mn+sd*expanse)
    ## case 3: draw curve with painted tail or body.
    ## case 3a: to paint lower tail/body set min<=(mn-sd*expanse) and max to desired cutpoint
    ## case 3b: to paint upper tail/body set man>=(mn+sd*expanse) and min to desired cutpoint
    if(min>max) stop("Error in midregion(): min must be less than or equal to max")
    lb <- mn-sd*expanse
    ub <- mn+sd*expanse
    x1=seq(lb,ub,length=200)
    y1=dnorm(x1,mean=mn,sd=sd)
    plot(x1,y1,type="n", main="", ylab="Proportion", xlab="X")
    if (!(min==max)){
        if(min<lb) min <- lb
        if(max>ub) max <- ub
        x2=seq(min,max,length=200)
        y2=dnorm(x2,mean=mn,sd=sd)
        ## browser()
        polygon(c(min,x2,max), c(0,y2,0), col=region.col, border=NA)
    }
    lines(x1, y1, type="l", lwd=lwd, col=line.col)
    ## TODO: do a better job with positioning the label
    if (label){ # calculate proportion under the curve and add label to plot
        area <- pnorm(max,mean=mn,sd=sd)-pnorm(min,mean=mn,sd=sd)
        if (min>lb) label.x <- min+.2*(max-min)
        else label.x <- max-.2*(max-min)
        label.y = max(y2)/5
        text(label.x, label.y, label=sprintf("%0.3f",area)) # TODO: drop leading zero
    }
}

##' Figure showing 68\%-95\%-99.7\% Rule for normal curve.
##'
##' @details nothing yet
##' @title Figure showing 68\%-95\%-99.7\% Rule for normal curve.
##' @param vlines logical. Use vertical lines to demarcate regions. Default=TRUE
##' @param colors character. A vector of 3 color names for each region.
##' @param xlab label for horizontal axis. Default = 'z'
##' @return used for side-effect of creating a plot
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
normalProps <- function(vlines=TRUE, colors=c("gray", "orange", "blue"), xlab="z") {
    x1=seq(-3.5,3.5,length=200)
    y1=dnorm(x1,mean=0,sd=1)
    par(cex=1.2, fin=c(6.375, 5), bg=NA)
    plot(x1,y1,type="n", main="", xlab=xlab, ylab="Proportion")
    ## ~99.73% of obs. within 3 sd of mean
    x2=seq(-3,3,length=200)
    y2=dnorm(x2)
    polygon(c(-3,x2,3),c(0,y2,0),col=colors[1], border=NA)
    ## ~95.45% of obs. within 2 sd of mean
    x2=seq(-2,2,length=200)
    y2=dnorm(x2)
    polygon(c(-2,x2,2),c(0,y2,0),col=colors[2], border=NA)
    ## ~68.27% of obs. within 1 sd of mean
    x2=seq(-1,1,length=200)
    y2=dnorm(x2)
    polygon(c(-1,x2,1),c(0,y2,0),col=colors[3], border=NA)
    lines(x1,y1,type="l",lwd=3,col="black")
    if (vlines) abline(v=c(-2,-1,0,1,2), lwd=2)
}

###########################################################################
##### Build a histogram matrix of random samples from normal dist.
##### with theoretical curves overlayed.
sampling <- function(nplots=12, nsamp=100, pop.mean=100, pop.sd=15, binwidth=pop.sd/3,
                         hist.fill.col='red', hist.line.col='black', hist.alpha=.5,
                         line.col='blue', line.size=1, line.alpha=.7) {
    sampv <- rnorm(nplots*nsamp, mean=pop.mean, sd=pop.sd) # get samples
    id <- rep(paste("X", 1:nplots, sep=""), each=nsamp) # an indicator variable
    df1 <- data.frame(sampv,id)
    ## build the histogram matrix
    p1 <- ggplot2::ggplot(data=df1, ggplot2::aes(x=sampv, y=..count..))
    p1 <- p1 + ggplot2::geom_histogram(binwidth=binwidth, fill=hist.fill.col, col=hist.line.col, alpha=hist.alpha) +
        ggplot2::facet_wrap(~id, ncol=ceiling(sqrt(nplots)))
    ## p1 <- p1 + theme(axis.text.x=element_text(hjust=-.05, angle=-45)) # TODO: figure out how to use themes.
    ## add normal curves
    x=rep(seq(pop.mean-pop.sd*4,pop.mean+pop.sd*4,length=200), nplots)
    y=rep(dnorm(x,mean=pop.mean,sd=pop.sd), nplots)*(binwidth*nsamp) # pop. normal curve, scaled to sample via binwidth*samplesize
    id <- rep(paste("X", 1:nplots, sep=""), each=200) # indicator, TODO: pad with leading 0s (X01 instead of X1) so that sort order when plotted is better
    df2 <- data.frame(x,y,id)
    p1 <- p1 + ggplot2::geom_line(data=df2,ggplot2::aes(x=x,y=y), col=line.col, alpha=line.alpha, size=line.size)
    p1 <- p1+ggplot2::ggtitle(bquote(paste(.(nplots), " samples (",n == .(nsamp), ") from Pop. (", mu == .(pop.mean),"; ", sigma == .(pop.sd),  ")")))
    ## get ranges for x and y axes
    r <- ggplot2::ggplot_build(p1) # ggplot_build() returns the object that is invisibly returned by ggplot2's print method, but without drawing the plot.
    ## Annotate facets with sample summary stats.
    ## Assumes all facets are on same scale. TODO: adapt for free scales.
    df1.sm <- plyr::ddply(df1, .(id), function(df) {data.frame(mean=mean(df$sampv), sd=sd(df$sampv), id=unique(df$id))})
    xmin <- r$panel$ranges[[1]]$x.range[1]; xmax <- r$panel$ranges[[1]]$x.range[2]
    df1.sm$xloc <- locate(xmin, xmax, .90)
    ymin <- r$panel$ranges[[1]]$y.range[1]; ymax <- r$panel$ranges[[1]]$y.range[2]
    df1.sm$yloc1 <- locate(ymin, ymax, .95)
    df1.sm$yloc2 <- locate(ymin, ymax, .88)
    p1 <- p1 + ggplot2::geom_text(data=df1.sm, inherit=F,
                         ggplot2::aes(x=xloc, y=yloc1, label=paste("M =", sprintf("%.1f", mean))),
                         size=2.5, hjust=1)
    p1 <- p1 + ggplot2::geom_text(data=df1.sm, inherit=F,
                         ggplot2::aes(x=xloc, y=yloc2, label=paste("SD =", sprintf("%.1f", sd))),
                         size=2.5, hjust=1)
    p1
}
##' Translate relative coordinates (pos) to data coords.
##'
##' This is a utility function for a translating relative coordinates (pos) to data coordinates. min and max
##' are values specifiying the full range of a plotting region on the data scale. pos is a
##' proportion. retval is data-scaled coordinate corresponding to pos.
##' @seealso See \code{\link[scales]{rescale}} as a possible alternative.
##' @title Translate relative coordinates (pos) to data coords.
##' @param min minimum bound of plotting region
##' @param max maximum bound of plotting region
##' @param pos a proportion to be located within min/max
##' @return used for side-effect of creating a plot
##' @author Dave Braze \email{davebraze@@gmail.com}
locate <- function(min, max, pos){
    pos*(max-min)+min
}

if (FALSE) {
#########################
#### correlation demo
#### TODO
#### 1. wrap into a pair of functions:
####    o one to build a data.frame containing n data sets with specific intercorrelations. See
####      vignette 13-simulate-data for method. Resulting data.frame should be in 'long' format.
####    o second function to plot the data with some options.

### positive associations
    n <- 100
    x <- rnorm(n)*3

    y0 <- rnorm(n)*6
    y1 <- x+rnorm(n, sd=6)
    y2 <- x+rnorm(n, sd=4.5)
    y3 <- x+rnorm(n, sd=3)
    y4 <- x+rnorm(n, sd=1.75)
    y5 <- x
    df0 <- data.frame(list(set=rep(LETTERS[1:6], each=n), x=rep(x,6), y=c(y0,y1,y2,y3,y4,y5)))

### negative associations (mirror image)
    y0r <- y0*-1
    y1r <- y1*-1
    y2r <- y2*-1
    y3r <- y3*-1
    y4r <- y4*-1
    y5r <- y5*-1
    df1 <- data.frame(list(set=rep(LETTERS[7:12], each=n), x=rep(x,6), y=c(y0r,y1r,y2r,y3r,y4r,y5r)))

    df3 <- rbind(df0,df1)

    p23 <- ggplot2::ggplot(data=df3, ggplot2::aes(x=x,y=y)) + ggplot2::geom_point(color="blue", alpha=.4) + ggplot2::facet_wrap(~ set, nrow = 2)

    ## Annotate facets with sample summary stats.
    df3.r <- plyr::ddply(df3, .(set), function(df) {data.frame(r=cor(df[,2:3])[1,2], set=unique(df$set))})
    p23build <- ggplot2::ggplot_build(p23)
    xmin <- p23build$panel$ranges[[1]]$x.range[1]; xmax <- p23build$panel$ranges[[1]]$x.range[2]
    df3.r$xloc <- locate(xmin, xmax, .90)
    ymin <- p23build$panel$ranges[[1]]$y.range[1]; ymax <- p23build$panel$ranges[[1]]$y.range[2]
    df3.r$yloc1 <- locate(ymin, ymax, .95)
    p23 <- p23 + ggplot2::geom_text(data=df3.r, inherit=F,
                           ggplot2::aes(x=xloc, y=yloc1, label=paste("r =", sprintf("%.2f", r))),
                           size=2.5, hjust=1)
    p23

}


if(FALSE){
######################################################################
### exam 4 plots
    n <- 25
    x <- rnorm(n, m=20, s=3)

    y1 <- rnorm(n, m=20, s=6)

    y2 <- x+rnorm(n, s=3)
    y3 <- x+rnorm(n, s=1)
    y4 <- ((x+rnorm(n, s=3))*-1)+40


    df3 <- data.frame(list(set=rep(LETTERS[1:4], each=n), X=rep(x,4), Y=c(y1,y2,y3,y4)))


    p23 <- ggplot2::ggplot(data=df3, ggplot2::aes(x=X,y=Y)) + ggplot2::geom_point(color="grey40") + ggplot2::facet_wrap(~ set, nrow = 1)

    ## Annotate facets with sample summary stats.
    df3.r <- plyr::ddply(df3, .(set), function(df) {data.frame(r=cor(df[,2:3])[1,2], set=unique(df$set))})
    p23build <- ggplot_build(p23)
    xmin <- p23build$panel$ranges[[1]]$x.range[1]; xmax <- p23build$panel$ranges[[1]]$x.range[2]
    df3.r$xloc <- locate(xmin, xmax, .90)
    ymin <- p23build$panel$ranges[[1]]$y.range[1]; ymax <- p23build$panel$ranges[[1]]$y.range[2]
    df3.r$yloc1 <- locate(ymin, ymax, .95)
    ## p23 <- p23 + geom_text(data=df3.r, inherit=F,
    ##                      ggplot2::aes(x=xloc, y=yloc1, label=paste("r =", sprintf("%.2f", r))),
    ##                      size=2.5, hjust=1)
    p23
}


