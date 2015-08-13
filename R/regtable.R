##' @title Customized regression table.
##'
##' No details yet...
##' @return A formatted regression table.
##' @author David Braze \email{davebraze@@gmail.com}
regtable <-
function (lmobj, intercept = FALSE, ...)
{
  # much of what follows is cropped from summmary.lm()

  if(!(class(lmobj) == 'lm')) {
    stop("lmobj is not of class 'lm'.")
  }

  z <- lmobj
  p <- z$rank

  if (p == 0) {
    stop("Case of 0-rank model not handled.")
  }

  Qr <- lmobj$qr
  if (is.null(z$terms) || is.null(Qr))
    stop("invalid 'lm' lmobj:  missing 'terms' or 'qr' components")

  ans <- list()
  ans$model <- deparse(z$terms)
  ans$names <- names(z$coef)
  ans$coef <- z$coef

  n <- NROW(Qr$qr)
  rdf <- n - p
  if (is.na(z$df.residual) || rdf != z$df.residual)
    warning("residual degrees of freedom in lmobj suggest this is not an \"lm\" fit")
  p1 <- 1:p
  r <- z$residuals
  f <- z$fitted
  w <- z$weights
  if (is.null(w)) {
    mss <- if (attr(z$terms, "intercept"))
      sum((f - mean(f))^2)
    else sum(f^2)
    rss <- sum(r^2)
  }
  else {
    mss <- if (attr(z$terms, "intercept")) {
      m <- sum(w * f/sum(w))
      sum(w * (f - m)^2)
    }
    else sum(w * f^2)
    rss <- sum(w * r^2)
    r <- sqrt(w) * r
  }
  resvar <- rss/rdf
  R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  se <- sqrt(diag(R) * resvar)
  est <- z$coefficients[Qr$pivot[p1]]
  tval <- est/se
  pval <- 2 * pt(abs(tval), rdf, lower.tail = FALSE)

  ans$tval <- tval
  ans$pval <- pval
  ans$r.squared <- mss/(mss+rss)

  if(!intercept) {
    labs <- c("names","coef","tval","pval")
    for (l in labs) {
      ans[[l]] <- ans[[l]][-1]
    }
  }

  for (cname in ans$names) {
    ans$sspc[which(ans$names == cname)] <-
      ans$r.squared - summary(update(z, formula(paste(".~. -", cname))))$r.squared
  }

  tab <- data.frame(ans[c(3:5,7)])
  rownames(tab) <- ans$names

  cat(ans$model, "\n\n", sep="")
  print(tab)
  cat(sprintf("\nMultiple R2: %0.5f\n", ans$r.squared))

  invisible(ans)

}
