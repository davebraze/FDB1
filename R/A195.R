##' @title Read a 'merge' file (*.mer), as might be exported from filemaker.
##'
##' @description Read a filemaker 'merge' file (*.mer). Filemaker *.mer files are CSV files.
##'
##' @details
##' This is a wrapper around \code{\link{read.csv}} to simplify reading tables exported from
##' Filemaker as 'merge' files.
##'
##' @param filename A *.mer file, as generated by filemaker.
##' @param columns Columns to select from *.mer file.
##' @param ... Additional arguments passed to \code{\link{read.csv}}.
##' @return A data.frame
##' @author Jon Gordils
##' @author David Braze \email{davebraze@@gmail.com}
##' @seealso
##' \code{\link{read.csv}}
##' \code{\link{read.table}}
##' @export
read.mer <- function(filename, columns, ...){
    retval <- read.csv(filename, ...)
    ## Check to ensure that all requested columns are in the *.mer file. If not, then stop.
    if(!all(columns %in% colnames(retval))){
        stop(paste(columns[!(columns %in% colnames(retval))], "not in", filename, sep = " ", collapse = " & "))
    }
    retval[columns]
}

