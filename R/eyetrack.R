##' @include be.numeric.R

library(stringr)

##' Used by read.ELascii(). Not intended for end-users. Extract fixations, saccades, and blinks from a trial.
##' @title Used by read.ELascii(). Not intended for end-users.
##' @param bounds A numeric tuple. e1 is index marking beginning of trial. e2 is index indicating
##' end of trial.
##' @param lines A vector of strings, each corresponding to 1 line of the EL ASCII file.
##' @return A list of 4 elements, data.frames enumerating fixations, saccades, blinks and TRIAL_VARs for the
##' trial.
##' @author Dave Braze \email{davebraze@@gmail.com}
getEyelinkTrialData <- function(bounds, lines) {
    fix <- grep("^EFIX", lines[bounds[1]:bounds[2]], value=TRUE)
    fix <- str_split(fix, pattern="[ \t]+")
    fix <- data.frame(matrix(unlist(fix), ncol=length(fix[[1]]), byrow=TRUE), stringsAsFactors=FALSE)
    toN <- sapply(fix, function(v) all(be.numeric(v)))
    fix <- data.frame(sapply(fix[!toN], as.factor, simplify=FALSE), sapply(fix[toN], as.numeric, simplify=FALSE))
    names(fix) <- c('event', 'eye', 'stime', 'etime', 'dur', 'xpos', 'ypos', 'pupil')
    fix$event <- gsub("^E", "", fix$event)

    sacc <- grep("^ESACC", lines[bounds[1]:bounds[2]], value=TRUE)
    sacc <- str_split(sacc, pattern="[ \t]+")
    sacc <- data.frame(matrix(unlist(sacc), ncol=length(sacc[[1]]), byrow=TRUE), stringsAsFactors=FALSE)
    toN <- sapply(sacc, function(v) all(be.numeric(v)))
    sacc <- data.frame(sapply(sacc[!toN], as.factor, simplify=FALSE), sapply(sacc[toN], as.numeric, simplify=FALSE))
    names(sacc) <- c('event', 'eye', 'stime', 'etime', 'dur', 'xpos1', 'ypos1', 'xpos2', 'ypos2', 'amp', 'QpupilQ')
    sacc$event <- gsub("^E", "", sacc$event)

    blink <- grep("^EBLINK", lines[bounds[1]:bounds[2]], value=TRUE)
    blink <- str_split(blink, pattern="[ \t]+")
    blink <- data.frame(matrix(unlist(blink), ncol=length(blink[[1]]), byrow=TRUE), stringsAsFactors=FALSE)
    toN <- sapply(blink, function(v) all(be.numeric(v)))
    blink <- data.frame(sapply(blink[!toN], as.factor, simplify=FALSE), sapply(blink[toN], as.numeric, simplify=FALSE))
    names(blink) <- c('event', 'eye', 'stime', 'etime', 'dur')
    blink$event <- gsub("^E", "", blink$event)

    trialvar <- grep("TRIAL_VAR", lines[bounds[1]:bounds[2]], value=TRUE)
    trialvar <- str_split(trialvar, pattern="[ \t]+")
    trialvar <- t(matrix(unlist(trialvar), ncol=length(trialvar[[1]]), byrow=TRUE)[,5:6])
    hdr <- trialvar[1,]
    trialvar <- data.frame(rbind(trialvar[2,]), stringsAsFactors=FALSE)
    names(trialvar) <- hdr
    toN <- sapply(trialvar, function(v) all(be.numeric(v)))
    trialvar <- data.frame(sapply(trialvar[!toN], as.factor, simplify=FALSE), sapply(trialvar[toN], as.numeric, simplify=FALSE))

    ## TODO: Pick up events flagged in MSG lines like the following.
    ## MSG	15334285 52 !V ARECSTART 0 1950006-letters2.wav
    ## critical information is
    ## o timestamp (15334285)
    ## o offset (52)
    ## o event type (ARECSTART)
    ## o modifier (1950006-letters2.wav)
    ##
    ## To make this work will need to pass in a list of regexp, each of which uniquely identifies an
    ## event of interest.
    ##
    ## Should these events be placed with trialvars, or in their own structure?

    ## TODO: Get sample level data put in separate list item.

    ## TODO: store trial start time, timestamp for START event (as a TRIAL_VAR?)

    retval <- list(fix=fix, sacc=sacc, blink=blink, trialvar=trialvar)
    retval
}



##' SR Research provides a utility (EDF2ASC.exe) that dumps ASCII renderings of their proprietary
##' EDF data file format. This function reads those ASCII files and extracts eye-movement events
##' (fixations, saccades, blinks) and TRIAL_VARs from them.
##' @title Get events from SR Research ASCII data files.
##' @param file A string giving path/fname to input file (ELalscii file).
##' @param tstartre A string containing regular expression that uniquely identifies beginning of trial.
##' @param tendre A string containing regular expression that uniquely identifies end of trial.
##' @param eye Indicates which eye ("R"|"L") to get events from. Currently unused.
##' @return List with one element for the file header and one element for each trial. Each trial
##' element is itself a list of 4 elements: data.frames enumerating fixations, saccades, blinks and
##' TRIAL_VARs for the trial.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
read.ELascii <- function(file, tstartre="TRIALID", tendre="TRIAL_RESULT", eye=NA) {
    f <- file(file, "r", blocking=FALSE)
    lines <- readLines(f, warn=TRUE, n=-1)
    close(f)

    ## get session information from file header
    header <- grep("^[*][*] ", lines, value=TRUE)
    script <- unlist(str_split(grep("RECORDED BY", header, value=TRUE), "[ \t]+"))[4]
    sessdate <- unlist(str_split(grep("DATE:", header, value=TRUE), ": "))[2]
    srcfile <- unlist(str_split(grep("CONVERTED FROM", header, value=TRUE), " (FROM|using) "))[2]
    srcfile <- basename(srcfile)

    ## get start and end lines for each trial block
    tstart <- grep(tstartre, lines)
    tend <- grep(tendre, lines)
    stopifnot (length(tstart) == length(tend))
    trialidx <- cbind(tstart, tend)

    ## get trial IDs
    trialids <- unlist(str_split(grep("TRIALID", lines, value=TRUE), " TRIALID "))
    trialids <- trialids[seq(2, length(trialids), 2)]

    ## get events for each trial
    retval <- apply(trialidx, 1, getEyelinkTrialData, lines=lines)
    names(retval) <- trialids
    retval
}


if(FALSE) {
    fname <- "../inst/extdata/1950006-RAN.asc"

    debug(read.ELascii)
    e <- read.ELascii(fname)
    undebug(read.ELascii)

    names(e$'2')
    head(e$'2'$fix)
    head(e$'2'$sacc)
    head(e$'2'$blink)
    dim(e$'2'$trialvar)
    names(e$'2'$trialvar)

}
