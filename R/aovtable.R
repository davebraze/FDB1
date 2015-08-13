##' @title Wrapper around \code{\link[stats]{aov}} with better output.
##' Wrapper around \code{\link[stats]{aov}} for repeated measures anova.
##'
##' @param formula : rhs of formula should be in partheses, () between any predictor and unit: y~(x1+x2)
##' @param unit : factor for stratification units, typicall subject or item, used to build error term
##' @param cv : say something here
##' @param data : data.frame passed to \code{\link[stats]{aov}}
##' @param dbg : enable verbose output for debugging
##' @param ... : additional arguments passed to \code{\link[stats]{aov}}
##' @return an anova table (data.frame)
##' @author David Braze \email{davebraze@@gmail.com}
##' @seealso \code{\link[stats]{aov}}
##' @family aovtable
aovtable <-
function(formula=NULL, unit=NULL, cv=NULL, data=NULL, dbg=FALSE, ...) {

  if(is.null(formula)) stop ("formula must be specified\n")
  if(!inherits(formula, "formula")) stop("Bad formula.")
  if(is.null(data)) stop ("data must be specified\n")

  if (!is.null(unit)){
    form <- formula(paste(deparse(formula(formula)), "*", unit))
    mod <- aov(formula=form, data=data)

    if (dbg) {
      print(form)
      print (summary(mod))
    }

    mss <- sum(fitted(mod)^2)             # model SS
    rss <- sum(residuals(mod)^2)          # residual SS
    r.squared <- mss/(mss+rss)
    tab <- summary(mod)
    tab <- tab[[1]]

    rxp <- paste("(", unit, "|Residuals)", sep="")
    tab.f <- tab[-grep(rxp, rownames(tab)),]
    ## print(tab)
    result <- matrix(nrow=dim(tab.f)[1], ncol=6)
    colnames(result) <- c("Df1", "SS1", "MS1", "Df2", "SS2", "MS2")
    rownames(result) <- rownames(tab.f)

    nl <- regexpr(" ", rownames(result))
    rownames(result) <- strtrim(rownames(result), max(nl))

    result <- data.frame(result)
    result[,"Df1"] <- tab.f[,"Df"]
    result[,"SS1"] <- tab.f[,"Sum Sq"]
    result[,"MS1"] <- tab.f[,"Mean Sq"]

    for (rr in rownames(result)){
      e <- grep(paste("^", gsub(" +", "", rr), ":",
                      unit, sep=""), rownames(tab))
      if(length(e)>0) {
        result[rr,4:6] <- tab[e,1:3]
      } else {
        result[rr,4:6] <- tab[grep("Residuals", rownames(tab)),1:3]
      }
      result[rr,"F value"] <- result[rr,"MS1"]/result[rr,"MS2"]
      result[rr,"Pr(>F)"] <- pf(result[rr, "F value"], result[rr,"Df1"], result[rr, "Df2"],
                                lower.tail=FALSE)
      result[rr,"eta sq"] <- result[rr, "SS1"]/(result[rr,"SS1"]+result[rr, "SS2"])
    } # end for()
    attr(result, "units") <- unit
  } else {
    ## print(noquote("  Units: None"))
    mod <- aov(formula=formula, data=data)

    mss <- sum(fitted(mod)^2)             # model SS
    rss <- sum(residuals(mod)^2)          # residual SS
    r.squared <- mss/(mss+rss)
    tab <- summary(mod)
    tab <- tab[[1]]
    ## print(tab)

    tab.f <- tab[-grep("Residuals", rownames(tab)),]
    result <- matrix(nrow=dim(tab.f)[1], ncol=6)
    colnames(result) <- c("Df1", "SS1", "MS1", "Df2", "SS2", "MS2")
    rownames(result) <- rownames(tab.f)

    nl <- regexpr(" ", rownames(result))
    rownames(result) <- strtrim(rownames(result), max(nl))
    result <- data.frame(result)

    result[,"Df1"] <- tab.f[,"Df"]
    result[,"SS1"] <- tab.f[,"Sum Sq"]
    result[,"MS1"] <- tab.f[,"Mean Sq"]

    for (rr in rownames(result)){
      result[rr,4:6] <- tab[grep("Residuals", rownames(tab)),1:3]
      result[rr,"F value"] <- result[rr,"MS1"]/result[rr,"MS2"]
      result[rr,"Pr(>F)"] <- pf(result[rr, "F value"], result[rr,"Df1"], result[rr, "Df2"],
                                lower.tail=FALSE)
      result[rr,"eta sq"] <- result[rr, "SS1"]/(result[rr,"SS1"]+result[rr, "SS2"])
    } # end for()
    attr(result, "units") <- ""

  } # end if ... else
  attr(result, "R2") <- sprintf("%0.5f", r.squared)
  attr(result, "formula") <- deparse(formula)
  result <- list(table=result, model=mod)
  class(result) <- c("aovtable", class(result))
  return(result)
}
