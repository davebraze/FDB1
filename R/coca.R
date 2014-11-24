##' A convenience wrapper around \code{\link{read.table}} with reasonable defaults for reading the
##' Corpus of Contemporary American English word frequency file (\url{corpus.byu.edu}). The file
##' contains tab delimited text, with some idiosynchracies.
##' @title Read CoCA word frequency table.
##' @param file per \code{\link{read.table}}
##' @param sep per \code{\link{read.table}}
##' @param na.strings per \code{\link{read.table}}
##' @param quote per \code{\link{read.table}}
##' @param header per \code{\link{read.table}}
##' @param simpleWC If TRUE then add vector of simplified wordclasses to data.frame. See
##' \code{\link{simpleWordClass}}.
##' @param ... additional arguments will be passed to \code{\link{read.table}}.
##' @return a data.frame
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @seealso \code{\link{read.table}}
##' @export
cocaReadFreq <- function(file, sep="\t", na.strings="  ", quote = "\"", header=TRUE, simpleWC=TRUE, ...) {
    D <- read.table(file, sep=sep, na.strings=na.strings, quote = quote, header=header, ...)
    if (simpleWC) {
        WC <- sapply(D$w1, simpleWordClass)
        D <- data.frame(WC, D)
    }
    D
}


##' CoCA uses a subset of wordclass tags from the CLAWS7 tagset
##' (http://ucrel.lancs.ac.uk/claws7tags.html). This function reduces those to a simplified set
##' @title Simplify CoCA (CLAWS7) part-of-speech tag.
##' @param wordclass One of the CLAWS7 tags used in CoCA
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
##' @param data A data.frame containing CoCA lexical statistics. See \code{\link{readCocaFreq}}.
##' @return A data.frame with columns w1, L1, WC.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
cocaForm2lemmas <- function(word, data) {
    d <- grep(paste("^", word, "$", sep=""), data$w1)
    lset <- data[d, c("w1", "L1", "WC")]
    retval <- unique(lset)
    retval
}


##' Given a lemma as a list containing a form/wordclass pair, return all wordforms associated with it
##' in CoCA.
##' @title Get attested wordforms for the specified lemma.
##' @param lemma A 2 element list with named elements "form" (the lemma form) and "class" (the
##' simple word class). For acceptable class values see \code{\link{simpleWordClass}}.
##' @param data A data.frame containing CoCA lexical statistics. See \code{\link{readCocaFreq}}.
##' @return A character vector containing wordforms.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
cocaLemma2forms <- function(lemma, data) {
    ii <- grep(paste("^", lemma$form, "$", sep=""), data$L1); retval <- data[ii,]
    ii <- grep(paste("^", lemma$class, "$", sep=""), retval$WC); retval <- retval[ii,]
    unique(as.character(retval$w1))
}
