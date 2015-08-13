##' Plot random effects from lmer fit.
##'
##' Create ggplot based QQ plot or caterpillar plot for \code{\link{ranef}} from \code{\link{mer}}
##' object.
##' @title Plot random effects from lmer fit.
##' @param re An object of class ranef.mer.
##' @param QQ If TRUE, create QQ plot. If FALSE create caterpillar plot.
##' @param likeDotplot Imitate dotplot() -> same scales for random effects.
##' @return A \code{\link{ggplot2}} object.
##' @author
##' \url{http://stackoverflow.com/users/484139/caracal}
##' \url{http://stackoverflow.com/users/1857266/didzis-elferts}
##' @seealso Original code cribbed from here:
##' \url{http://stackoverflow.com/questions/13847936/in-r-plotting-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot}
##' @export
ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
    f <- function(x) {
        pv   <- attr(x, "postVar")
        cols <- 1:(dim(pv)[1])
        se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
        ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
        pDf  <- data.frame(y=unlist(x)[ord],
                           ci=1.96*se[ord],
                           nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                           ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                           ind=gl(ncol(x), nrow(x), labels=names(x)))

        if(QQ) {  ## normal QQ-plot
            p <- ggplot2::ggplot(pDf, ggplot2::aes(nQQ, y))
            p <- p + ggplot2::facet_wrap(~ ind, scales="free")
            p <- p + ggplot2::xlab("Standard normal quantiles") + ggplot2::ylab("Random effect quantiles")
        } else {  ## caterpillar dotplot
            p <- ggplot2::ggplot(pDf, ggplot2::aes(ID, y)) + ggplot2::coord_flip()
            if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
                p <- p + ggplot2::facet_wrap(~ ind)
            } else {           ## different scales for random effects
                p <- p + ggplot2::facet_grid(ind ~ ., scales="free_y")
            }
            p <- p + ggplot2::xlab("Levels") + ggplot2::ylab("Random effects")
        }

        p <- p + theme(legend.position="none")
        p <- p + ggplot2::geom_hline(yintercept=0)
        p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
        p <- p + ggplot2::geom_point(ggplot2::aes(size=1.2), colour="blue")
        return(p)
    }

    lapply(re, f)
}
