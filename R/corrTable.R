
##' This is wrapper around \code{[Hmisc]{rcor2}} that aims to return a reasonably formatted table of
##' correlations. R values are in the lower triangle with NAs elsewhere, including the diagonal.
##'
##' TODO: put means or SDs on the diagonal; put Ns or pvals in the upper.tri
##' @title Get correlation table for numerical matrix M.
##' @param M : a numeric matrix with at least 2 columns
##' @param type : passed to Hmisc::rcor2
##' @param file : a string. If not empty, then save the matrix to file with the given name.
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
        diag(retval) <- sapply(D2, sd, na.rm=T)
    } else if ("var"==diag) {
        diag(retval) <- sapply(D2, var, na.rm=T)
    } else if ("R"==diag) {
        diag(retval) <- 1
    }

    if(""!=file) {
        write.table(retval, file=file, sep=",",
                    col.names=TRUE, na="", quote=FALSE)
    }
    retval
}
