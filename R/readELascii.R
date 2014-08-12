##' @include be.numeric.R

library(stringr)

if(FALSE) {
    fname <- "../data/195bj006-RAN.asc"
    file <- fname
}

##' Used by readELascii(). Not intended for end-users.
##'
##' Used by readELascii(). Not intended for end-users.
##' @title getEyeEvents()
##' @param bounds A numeric tuple. e1 is index marking beginning of trial. e2 is index indicating
##' end of trial.
##' @param lines A vector of strings, each corresponding to 1 line of the EL ASCII file.
##' @return List with one element for the file header and one element for each trial. Each trial
##' element is itself a list of 3 elements: data.frames enumerating fixations, saccades, and blinks
##' for the trial.
##' @author Dave Braze
getEyeEvents <- function(bounds, lines) {
    fix <- grep("^EFIX", lines[bounds[1]:bounds[2]], value=TRUE)
    fix <- str_split(fix, pattern="[ \t]+")
    fix <- data.frame(matrix(unlist(fix), ncol=length(fix[[1]]), byrow=TRUE), stringsAsFactors=FALSE)
    toN <- sapply(fix, function(v) all(be.numeric(v)))
    fix <- data.frame(sapply(fix[!toN], as.factor, simplify=FALSE), sapply(fix[toN], as.numeric, simplify=FALSE))

    sacc <- grep("^ESACC", lines[bounds[1]:bounds[2]], value=TRUE)
    sacc <- str_split(sacc, pattern="[ \t]+")
    sacc <- data.frame(matrix(unlist(sacc), ncol=length(sacc[[1]]), byrow=TRUE), stringsAsFactors=FALSE)
    toN <- sapply(sacc, function(v) all(be.numeric(v)))
    sacc <- data.frame(sapply(sacc[!toN], as.factor, simplify=FALSE), sapply(sacc[toN], as.numeric, simplify=FALSE))

    blink <- grep("^EBLINK", lines[bounds[1]:bounds[2]], value=TRUE)
    blink <- str_split(blink, pattern="[ \t]+")
    blink <- data.frame(matrix(unlist(blink), ncol=length(blink[[1]]), byrow=TRUE), stringsAsFactors=FALSE)
    toN <- sapply(blink, function(v) all(be.numeric(v)))
    blink <- data.frame(sapply(blink[!toN], as.factor, simplify=FALSE), sapply(blink[toN], as.numeric, simplify=FALSE))

    retval <- list(fix=fix, sacc=sacc, blink=blink)
    retval
}

##' 'readELascii' takes an ASCII data file created by SR Research's EDF2ASC utility and extracts
##' 'eye' events for each trial contained within that file.
##'
##' SR Research provides a utility (EDF2ASC.exe) that dumps ASCII renderings of their proprietary
##' EDF data file format. This function reads those ASCII files and extracts eye-movement events
##' from them (fixations, saccades, blinks).
##' @title Get events from SR Research ASCII data files
##' @param file string giving path/fname to input file (ELalscii file)
##' @param tstartre string containing regular expression that uniquely identifies beginning of trial
##' @param tendre string containing regular expression that uniquely identifies end of trial
##' @param eye indicates which eye ("R"|"L") to get events from. Currently unused.
##' @return List with one element for the file header and one element for each trial. Each trial
##' element is itself a list of 3 elements: data.frames enumerating fixations, saccades, and blinks
##' for the trial.
##' @author Dave Braze
##' @export
readELascii <- function(file="", tstartre="TRIALID", tendre="TRIAL_RESULT", eye=NA) {
    f <- file(file, "r", blocking=FALSE)
    lines <- readLines(f, warn=TRUE, n=-1)
    close(f)

    header <- grep("^[*][*] ", lines, value=TRUE)

    tstart <- grep(tstartre, lines)
    tend <- grep(tendre, lines)
    stopifnot (length(tstart) == length(tend))
    trialidx <- cbind(tstart, tend)

    retval <- apply(trialidx, 1, getEyeEvents, lines=lines)
    retval
}
