##' @title Plot random effects from lmer fit.
##'
##' @description Build a 'caterpillar' plot of lmer random effects.
##'
##' @details
##' Create ggplot based QQ plot or caterpillar plot for \code{\link[lme4]{ranef}} from \code{\link[lme4]{merMod}}
##' object.
##'
##' @param re An object of class ranef.mer.
##' @param QQ If TRUE, create QQ plot. If FALSE create caterpillar plot.
##' @param likeDotplot Imitate dotplot() -> same scales for random effects.
##' @return A \code{\link[ggplot2]{ggplot}} object.
##' @author
##' \url{http://stackoverflow.com/users/484139/caracal}
##' \url{http://stackoverflow.com/users/1857266/didzis-elferts}
##' @seealso Original code cribbed from here:
##' \url{http://stackoverflow.com/questions/13847936/in-r-plotting-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot}
##' @export
ggCaterpillar <- function(re,
                          QQ=TRUE,
                          likeDotplot=TRUE) {
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


##' @title Empirical and implied probabilities for binomial outcome models.
##'
##' @details
##' Returns a table containing columns:
##' \enumerate{
##'     \item First 4 columns as per summary(mod)$coefficients: Estimate, Std. Error, z value, Pr(>|z|)
##'     \item 'count' number of 1s in response vector
##'     \item 'n' length of response vector
##'     \item 'prob.emp' empirical probabilities
##'     \item 'prob.est.fe' model implied probabilities due to fixed effects only
##'     \item 'prob.est.re' model implied probabilities for combined fixed and random effects
##' }
##'
##' WARNING: this function is very fragile. It is easily broken.
##'
##' @param model A glmer object.
##' @param classes A character vector containing names of vectors for classification.
##' @param drop An integer vector indicating non-categorical terms so that they can be dropped.
##' @return A table consisting of terms built from categorical variables and their linear combinations
##' @author David Braze \email{davebraze@@gmail.com}
##' @export
glmerProb <- function(model, classes, drop=NA) {
    ## compare model fit (fixed effect logits) and model-implied probabilities to empirical probabilities
    predfe <- predict(model, type="response", re.form=NA)
    predre <- predict(model, type="response", re.form=NULL)
    model.d <- data.frame(model@frame, predfe, predre)
    ep <- plyr::ddply(model.d, classes, summarise,
                      count=sum(fpregres),        # number of 1s in response vector
                      n=length(fpregres),         # length of response vector
                      prob.emp=count/n,           # empirical probability
                      prob.est.fe=mean(predfe),   # model implied probability due to fixed effects only
                      prob.est.re=mean(predre))   # model implied probability for combined fixed and randome effects
    mp <- summary(model)$coefficients
    if(!is.na(drop)) {
        mp <- mp[-drop,]       # drop non-categorical terms
    }
    data.frame(mp, ep)
}
