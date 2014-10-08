
##' This is wrapper around \code{\link[Hmisc]{rcor2}} that aims to return a reasonably formatted table of
##' correlations. R values are in the lower triangle with NAs elsewhere. Diagonal is NA by default,
##' but may be set to standard deviations or variances.
##'
##' TODO: put means or SDs on the diagonal; put Ns or pvals in the upper.tri
##' @title Get correlation table for numerical matrix M.
##' @param M A numeric matrix with at least 2 columns.
##' @param type Passed to \code{\link[Hmisc]{rcor2}}.
##' @param file A string. If not empty, then save the matrix to file with the given name.
##' @param diag Values to put in diagonal of corrTable: NA (default), standard deviation, variance
##' @return A matrix with R values in the lower triangle and NAs elsewhere
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
corrTable <- function(M, type='pearson', file="", diag=FALSE) {

    rcor2 <- Hmisc::rcorr(as.matrix(M), type=type)
    retval <- rcor2$r
    retval[upper.tri(retval, diag=TRUE)] <- NA

    colnames(retval) <- as.character(1:length(colnames(retval)))
    cnames=colnames(retval)
    cnames[1] = paste(",", cnames[1], sep="")
    colnames(retval) <- cnames

    rownames(retval) <- paste(as.character(1:length(rownames(retval))), rownames(retval), sep=". ")

    if("sd"==diag){
        diag(retval) <- sapply(M, sd, na.rm=T) # this will bomb if M is a matrix
    } else if ("var"==diag) {
        diag(retval) <- sapply(M, var, na.rm=T) # this will bomb if M is a matrix
    } else if ("R"==diag) {
        diag(retval) <- 1
    }

    if(""!=file) {
        write.table(retval, file=file, sep=",",
                    col.names=TRUE, na="", quote=FALSE)
    }
    retval
}
