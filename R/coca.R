##' @details
##' Mostly a convenience wrapper around \code{\link{read.table}} with reasonable defaults for reading the
##' Corpus of Contemporary American English word frequency file (\url{corpus.byu.edu}). The file
##' contains tab delimited text, with some idiosynchracies.
##'
##' Contents of data.frame as documented in CoCA itself.
##'
##' The following information is adapted from the spreadsheet version of the lexical frequency table
##' that is distributed with CoCA itself.
##'
##' This spreadsheet contains the 100,000 word list (http://www.wordfrequency.info/100k.asp) that is
##' based on the Corpus of Contemporary American English (COCA; http://corpus.byu.edu/coca/) and
##' other corpora (http://corpus.byu.edu).
##'
##' This copy of the data cannot be shared with others. Note also that a small change has been made
##' to the data in this spreadsheet to indentify you as the source of the spreadsheet.
##'
##' The file includes a great deal of data from several different corpora. Column contents are
##' listed below, by column name.
##'
##' Column
##' \itemize{
##'   \item \code{WC}        Simplified word class, if requested. See \code{simpleWC} argument to this function.
##'   \item \code{ID}        Numerical word ID (rank order), 1-100,000
##'   \item \code{w1}        Word form
##'   \item \code{L1}        Lemma/stem (e.g. go for the words gone or went, or book for the word books, or quick for the word quicker)
##'   \item \code{c1}        Part of speech. This is the first letter from the codes at \url{http://ucrel.lancs.ac.uk/claws7tags.html}
##'   \item \code{pc}        Percent of tokens that are capitalized. This lets you see whether the word occurs mainly in proper noun-like contexts, like Ravens (the Baltimore Ravens team v the actual animal), March (the month vs. a walk), Brown (the surname vs the adjective), Beach (in place names like Daytona Beach), AIDS (the disease vs e.g. visual aids), or Rice (the university or surname vs the food). Note that some words have a high degree of capitalization simply because they occur primarily at the beginning of sentences, e.g. Hello or Unfortunately.
##'   \item \code{spelling}  Whether the word is an American or British spelling
##'   \item \code{coca}      Raw frequency (# tokens) in the 450 million word Corpus of Contemporary American English (\url{http://corpus.byu.edu/coca})
##'   \item \code{pcoca}     Frequency (per million words) in the 450 million word Corpus of Contemporary American English (\url{http://corpus.byu.edu/coca})
##'   \item \code{pbnc}      Frequency (per million words) in the 100 million word British National Corpus (\url{http://corpus.byu.edu/bnc})
##'   \item \code{psoap}     Frequency (per million words) in the 100 million word Corpus of American Soap Operas (\url{http://corpus2.byu.edu/soap})
##'   \item \code{ph3-ph1}   Frequency (per million words) in the Corpus of Historical American English (\url{http://corpus.byu.edu/coha}): 1950-1989, 1900-1949, and 1810-1899
##'   \item \code{pc1-pc5}   Frequency (per million words) in COCA genres: spoken, fiction, popular magazines, newspapers, and academic journals
##'   \item \code{pb1-pb7}   Frequency (per million words) in BNC genres: spoken, fiction, popular magazines, newspapers, non-academic journals, academic journals, and miscellaneous
##'   \item \code{tpcoca}    Percentage of COCA texts (0.00-1.00) that contain the word at least once.
##'   \item \code{tpbnc}     Percentage of BNC texts (0.00-1.00) that contain the word at least once.
##'   \item \code{tpsoap}    Percentage of SOAP texts (0.00-1.00) that contain the word at least once.
##'   \item \code{tph3-tpb7} Percentage of texts (0.00-1.00) that contain the word at least once: 1) COHA time periods   3) COCA genres   4) BNC genres
##'   \item \code{bnc-fb7}   Raw token frequency in BNC, SOAP, COHA, COCA genres and BNC genres: the basis for Columns pcoca through pb7
##'   \item \code{tcoca-tb7} Raw number of texts in COCA, BNC, SOAP, COHA, COCA genres and BNC genres: the basis for Columns tpcoca through tpb7
##' }
##'
##' @title Read CoCA word frequency table.
##' @param file Sent to \code{\link{read.table}}.
##' @param sep The CoCA lexical frequency file is tab delimited. Value sent to \code{\link{read.table}}.
##' @param na.strings Sent to \code{\link{read.table}}.
##' @param quote Some fields in CoCA file contain "'". So remove that character from the
##' \code{read.table} default for this parameter. Sent to \code{\link{read.table}}.
##' @param header The CoCA file includes a header. Value sent to \code{\link{read.table}}.
##' @param fill Over-ride default value because the end of the header row in the CoCA frequency file
##' has a stray tab, at least in my copy.
##' @param skip Skip 2 comment rows at the top of the file.
##' @param simpleWC If TRUE (the default) then add vector of simplified wordclasses to data.frame. See
##' \code{\link{cocaSimpleWordClass}}.
##' @param ... additional arguments will be passed to \code{\link{read.table}}.
##' @return a data.frame
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @seealso \code{\link{read.table}}
##' @export
cocaReadFreq <- function(file, sep="\t", na.strings="  ", quote = "\"", header=TRUE, fill=TRUE, skip=2, simpleWC=TRUE, ...) {
    retval <- read.table(file, sep=sep, na.strings=na.strings, quote = quote, header=header, fill=fill, skip=skip, ...)
    if (simpleWC) {
        WC <- sapply(as.character(retval$c1), cocaSimpleWordClass)
        retval <- data.frame(WC, retval)
    }
    retval
}


##' CoCA uses a subset of 54 wordclass tags from the CLAWS7 tagset
##' (\url{http://ucrel.lancs.ac.uk/claws7tags.html}). This function reduces those to a simplified set of
##' 15 tags. Not all will be useful.
##' @title Simplify CoCA (CLAWS7) part-of-speech tags.
##' @param wordclass One of the CLAWS7 tags used in CoCA.
##' @return A string containing the simplified wordclass tag.
##' @author  Dave Braze \email{davebraze@@gmail.com}
##' @export
cocaSimpleWordClass <- function(wordclass) {
    stopifnot(is.character(wordclass))
    stopifnot(length(wordclass)==1)
    trans <- matrix(c( "appge", "posspron", "at", "det", "at1", "det", "cc", "conj", "cs", "conj", "da",
                      "det", "db", "det", "dd", "det", "ex", "exist_there", "ge", "gen", "ii", "prep", "jj", "adj",
                      "jjr", "adj", "jjt", "adj", "mc", "number", "md", "number", "mf", "number", "nn1", "noun", "nn2",
                      "noun", "pn", "pron", "pp", "pron", "rr", "adverb", "rrr", "adverb", "rrt", "adverb", "to",
                      "inf_TO", "uh", "interj", "vb0", "verb", "vbdr", "verb", "vbdz", "verb", "vbg", "verb", "vbm",
                      "verb", "vbn", "verb", "vbr", "verb", "vbz", "verb", "vd0", "verb", "vdd", "verb", "vdg", "verb",
                      "vdn", "verb", "vdz", "verb", "vh0", "verb", "vhd", "verb", "vhg", "verb", "vhn", "verb", "vhz",
                      "verb", "vm", "verb", "vmk", "verb", "vv", "verb", "vv0", "verb", "vvd", "verb", "vvg", "verb",
                      "vvgk", "verb", "vvn", "verb", "vvz", "verb", "xx", "nt"),
                    ncol=2, byrow=TRUE)
    retval <- trans[wordclass == trans[,1],2]
    retval
}


##' Given a wordform, return all lemmas listed for that form in CoCA, as a 3 column data.frame
##' containing wordform/lemma/wordclass.
##' @title Get lemmas for the specified wordform.
##' @param word A character string holding the wordform
##' @param data A data.frame containing CoCA lexical statistics. See \code{\link{cocaReadFreq}}.
##' @return A character matrix with columns w1 (wordform), L1 (lemma/stem), WC (word class).
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
cocaWord2lemmas <- function(word, data) {
    d <- grep(paste("^", word, "$", sep=""), data$w1)
    lset <- data[d, c("w1", "L1", "WC")]
    retval <- unique(lset)
    retval <- as.matrix(retval)
    rownames(retval) <- NULL
    retval
}


##' Given a lemma as a list containing a form/wordclass pair, return all wordforms associated with it
##' in CoCA.
##' @title Get attested wordforms for the specified lemma.
##' @param lemma A 2 element character vector. The first element is the lemma form, and the second element is the
##' simple word class. See \code{\link{cocaSimpleWordClass}} for acceptable word class values.
##' @param data A data.frame containing CoCA lexical statistics. See \code{\link{cocaReadFreq}}.
##' @return A character vector containing wordforms.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
cocaLemma2words <- function(lemma, data) {
    ii <- grep(paste("^", lemma[1], "$", sep=""), data$L1)
    retval <- data[ii,]
    ii <- grep(paste("^", lemma[2], "$", sep=""), retval$WC)
    retval <- retval[ii,]
    retval <- unique(as.character(retval$w1))
    retval
}

##' Return the set of wordforms associated with a lemma.
##'
##' @details Return the set of wordforms associated with a lemma. If wordclass (WC) is specified,
##' then the set is restricted to that wordclass.
##' @title Return the set of wordforms associated with a lemma.
##' @param lemma Character string indicating the lemma (form).
##' @param WC Character string indicating the wordclass of the lemma.
##' @param data A data.frame containing CoCA lexical statistics. See \code{\link{cocaReadFreq}}.
##' @return A data.frame containing columns for lemma (L1), simpleWC (WC), CLAWS7 tag (c1), wordform
##' (w1), coca frequency (coca).
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
cocaLemmaSet <- function(lemma="run", WC=NA, data) {
    if(is.na(WC)) {
        lset <- data[data$L1==lemma, c("L1", "WC", "c1", "w1", "coca")]
    } else {
        lset <- data[data$L1==lemma & data$WC==WC, c("L1", "WC", "c1", "w1", "coca")]
    }
    retval <- lset[order(lset$WC, lset$c1),]
    retval
}

if (FALSE) {
    lemmas <- c("run", "hunt", "race")
    plyr::ldply(lemmas, cocaLemmaSet, WC="verb", data=D)
    plyr::ldply(lemmas, cocaLemmaSet, data=D)
}

##' Summed wordform frequencies for a lemma.
##'
##' @details Return the summed wordform frequencies for a lemma. If wordclass (WC) is specified,
##' then the set is restricted to that wordclass.
##'
##' TODO: Fix content of WC in case of multiple WC (WC=NA). Should be concatenation of actual
##' values.
##' @title Summed wordform frequencies for a lemma.
##' @param lemma Character string indicating the lemma (form).
##' @param WC Character string indicating the wordclass of the lemma.
##' @param data A data.frame containing CoCA lexical statistics. See \code{\link{cocaReadFreq}}.
##' @return A data.frame containing columns for lemma (L1), simpleWC (WC), number of simpleWC
##' included in summary (nWC), number of wordforms included in summary (nforms), summed coca
##' frequencies for wordforms (coca).
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
cocaLemmaFreq <- function(lemma="run", WC=NA, data) {
    lset <- cocaLemmaSet(lemma, WC, data)
    nforms <- dim(lset)[1]
    nWC <- length(unique(lset$WC))
    freq <- sum(lset$coca, na.rm=TRUE)

    if (!is.na(WC)) {
        retval <- data.frame(lemma, WC, nWC, nforms, freq)
    } else {
        retval <- data.frame(lemma, WC, nWC, nforms, freq)
    }
    retval
}

if(FALSE) {
    cocaLemmaFreq("run", WC="verb", data=D)
    cocaLemmaFreq("run", WC=NA, data=D)
    lemmas <- c("run", "hunt", "race")
    plyr::ldply(lemmas, cocaLemmaFreq, WC="verb", data=D)
    plyr::ldply(lemmas, cocaLemmaFreq, WC=NA, data=D)
}

##' Find lexical neighbors.
##'
##' @details Find all neighbors for \code{word} in data (assumes coca data.frame). Neighbors are defined as words
##' differing from \code{word} by a single letter substitution, deletion or insertion.
##' @title Find lexical neighbors.
##' @param word A character string indicating the word defining the neighborhood.
##' @param data  A data.frame containing CoCA lexical statistics. See \code{\link{cocaReadFreq}}.
##' @return Returns a data.frame containing 1 row for each neighbor found in \code{data},
##' specifying: index (ID), wordform (w1), lemma (L1), simple wordclass (WC), CLAWS7 tag (c1), raw
##' CoCA frequency (coca).
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
cocaNeighbors <- function(word, data) {
    ## Find all neighbors for _word_ in data (assumes coca data.frame).
    s <- strsplit(word, "")[[1]]
    slen <- length(s)

    ## find neighbors by substitution
    substRE <- matrix(s, nrow=slen, ncol=slen, byrow=TRUE)
    diag(substRE) <- "."
    substRE <- apply(substRE, 1, paste0, collapse="")
    substRE <- paste0("^", substRE, "$", collapse="|")
    substID <- grep(substRE, D$w1)

    ## find neighbors by deletion
    delRE <- matrix(s, nrow=slen, ncol=slen, byrow=TRUE)
    keep <- sort(c(which(upper.tri(delRE)), which(lower.tri(delRE))))
    delRE <- matrix(delRE[keep], nrow=slen)
    delRE <- apply(delRE, 1, paste0, collapse="")
    delRE <- paste0("^", delRE, "$", collapse="|")
    delID <- grep(delRE, D$w1)

    ## find neighbors by insertion
    insRE <- matrix(nrow=slen+1, ncol=slen+1)
    tmp <- matrix(s, nrow=slen, ncol=slen, byrow=TRUE)
    diag(insRE) <- "."
    insRE[upper.tri(insRE)] <- tmp[upper.tri(tmp, diag=T)]
    insRE[lower.tri(insRE)] <- tmp[lower.tri(tmp, diag=T)]
    insRE <- apply(insRE, 1, paste0, collapse="")
    insRE <- paste0("^", insRE, "$", collapse="|")
    insID <- grep(insRE, D$w1)

    nHoodID <- unique(c(substID, delID, insID))
    neighbors <- D[nHoodID, c("ID", "w1", "L1", "WC", "c1", "coca")]
    neighbors
}

if(FALSE){
    cocaNeighbors("sand", D)
}

##' Get lexical cohort.
##'
##' @details Find all orthographic cohort members for \code{word} in data (assumes coca
##' data.frame). Cohort members share \code{onset.len} initial characters.
##' @title Get lexical cohort.
##' @param word A character string indicating the word defining the cohort.
##' @param data  A data.frame containing CoCA lexical statistics. See \code{\link{cocaReadFreq}}.
##' @param onset.len The number of onset letters that must cohort members share.
##' @return Returns a data.frame containing 1 row for each neighbor found in \code{data},
##' specifying: index (ID), wordform (w1), lemma (L1), simple wordclass (WC), CLAWS7 tag (c1), raw
##' CoCA frequency (coca).
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
cocaCohort <- function(word, data, onset.len=1) {
## Find all orthographic cohort members for _word_ in data (assumes coca
## data.frame). Cohort members share \code{onset.len} initial characters.
##
## Returns a data.frame containing 1 row for each cohort member found in _data_, specifying:  index (ID),
## wordform (w1), lemma (L1), simple wordclass (WC), CLAWS7 tag (c1), raw CoCA frequency (coca).
    s <- paste0(strsplit(word, "")[[1]][1:onset.len], collapse="")
    coh1re <- paste0("^", s)
    cohort1idx <- grep(coh1re, data$w1)
    cohort1 <- data[cohort1idx,c("ID", "w1", "L1", "WC", "c1", "coca")]
    cohort1
}

if(FALSE){
    coh1 <- cocaCohort("sand", D)
    coh2 <- cocaCohort("sand", D, onset.len=2)
}


