##' @title Function for plotting eye-movement summaries by sentence region.
##'
##' iplot() is based on interaction.plot(). Be careful of missing values in "response"; make sure
##' that "fun" does the right thing with them.
##' @param x.factor Factor for x axis, usually sentence region.
##' @param trace.factor Factor for separate traces, usually sentence condition.
##' @param response Response measure to be plotted on y axis.
##' @param fun Function for summarizing response measure, defaults to mean(x, na.rm=T).
##' @param type This and following params are as per plot() and par(), which see.
##' @param legend
##' @param leg.title
##' @param trace.labels
##' @param fixed
##' @param xlab
##' @param ylab
##' @param main
##' @param ylim
##' @param lty
##' @param lwd
##' @param col
##' @param pch
##' @param xpd
##' @param leg.bg
##' @param leg.bty
##' @param point.cex
##' @param leg.x
##' @param leg.y
##' @param xtick
##' @param xaxt
##' @param axes
##' @param ... other parameters that will be passed to matplot()
##' @return None. Used for its side effects.
##' @author David Braze \email{davebraze@@gmail.com}
##' @export
iplot <-
function (x.factor, trace.factor, response, fun = function(x){mean(x, na.rm=TRUE)}, type =
c("l", "p", "b", "o"), legend = TRUE, leg.title = deparse(substitute(trace.factor)), trace.labels =
NULL, fixed = FALSE, xlab = deparse(substitute(x.factor)), ylab = ylabel, main="", ylim =
range(cells, na.rm = TRUE), lty = nc:1, lwd = 1, col = 1, pch = c(1:9, 0, letters), xpd = NULL,
leg.bg = par("bg"), leg.bty = "n", point.cex=1, leg.x = 1, leg.y = ylim[2], xtick = FALSE, xaxt =
par("xaxt"), axes = TRUE, ...)  {

  ylabel <- paste(deparse(substitute(fun)), "of ", deparse(substitute(response)))
  type <- match.arg(type)
  cells <- tapply(response, list(x.factor, trace.factor), fun)
  nr <- nrow(cells)
  nc <- ncol(cells)
  xvals <- 1:nr
  if (is.ordered(x.factor)) {
    wn <- getOption("warn")
    options(warn = -1)
    xnm <- as.numeric(levels(x.factor))
    options(warn = wn)
    if (!any(is.na(xnm)))
      xvals <- xnm
  }
  xlabs <- rownames(cells)

  # set trace labels
  if(is.null(trace.labels))
    ylabs <- colnames(cells)
  else
    ylabs <- trace.labels

  nch <- max(sapply(ylabs, nchar))
  if (is.null(xlabs))
    xlabs <- as.character(xvals)
  if (is.null(ylabs))
    ylabs <- as.character(1:nc)
  xlim <- range(xvals)

  xlim <- xlim + c(-0.2/nr, 0.2/nr) * diff(xlim)   # fdb mods to legend handling

  yrng <- diff(ylim)

  matplot(xvals, cells, ..., type = type, xlim = xlim, ylim = ylim,
          xlab = xlab, ylab = ylab, axes = axes, xaxt = "n", col = col,
          lty = lty, lwd = lwd, pch = pch, cex=point.cex)

  if (axes && xaxt != "n") {
    mgp. <- par("mgp")
    if (!xtick)
      mgp.[2] <- 0
    axis(1, at = xvals, labels = xlabs, tick = xtick, mgp = mgp.,
         xaxt = xaxt, ...)
  }
  title(main=main)
  if (legend) {
    if (!is.null(xpd) || {
      xpd. <- par("xpd")
      !is.na(xpd.) && !xpd. && (xpd <- TRUE)}) {
      op <- par(xpd = xpd)
      on.exit(par(op))
    }
    text(leg.x, leg.y, paste("  ", leg.title), adj = 0)
    if (!fixed) {
      ord <- sort.list(cells[nr, ], decreasing = TRUE)
      ylabs <- ylabs[ord]
      lty <- lty[1 + (ord - 1)%%length(lty)]
      col <- col[1 + (ord - 1)%%length(col)]
      pch <- pch[ord]
    }
    if (type %in% c("l"))
      legend(leg.x, leg.y, legend = ylabs, col = col, lty = lty, lwd = lwd,
             bty = leg.bty, bg = leg.bg)
    if (type %in% c("p"))
      legend(leg.x, leg.y, legend = ylabs, col = col, pch = pch,
             bty = leg.bty, bg = leg.bg)
    if (type %in% c("o", "b"))
      legend(leg.x, leg.y, legend = ylabs, col = col, pch = pch,
             lty = lty, lwd = lwd, bty = leg.bty, bg = leg.bg)
  }
  invisible(NULL)
}
