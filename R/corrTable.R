library(Hmisc)

##' Get correlation table for numerical matrix M.
##'
##' This is wrapper around Hmisc::rcor2() that aims to return a reasonably formatted table of
##' correlations. R values are in the lower triangle with NAs elsewhere, including the diagonal.
##'
##' TODO: put means or SDs on the diagonal; put Ns or pvals in the upper.tri
##' @title Get correlation table for numerical matrix M.
##' @param M a numeric matrix with at least 2 columns
##' @param type passed to Hmisc::rcor2
##' @param file a string. If not empty, then save the matrix to file with the given name.
##' @return A matrix with R values in the lower triangle and NAs elsewhere
##' @author Dave Braze
corrTable.R <- function(M, type, file="") {

    rcor2 <- rcorr(as.matrix(M), type="pearson")
    retval <- rcor2$r
    retval[upper.tri(retval, diag=TRUE)] <- NA

    colnames(retval) <- as.character(1:length(colnames(retval)))
    cnames=colnames(retval)
    cnames[1] = paste(",", cnames[1], sep="")

    rownames(retval) <- paste(as.character(1:length(rownames(retval))), vars, sep=". ")
    retval <- retval[,-dim(retval[2])]

    if(file) {
        write.table(retval, file=file, sep=",",
                    col.names=cnames[-length(cnames)], na="", quote=FALSE)
    }
}
