% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coca.R
\name{cocaCohort}
\alias{cocaCohort}
\title{Get lexical cohort.}
\usage{
cocaCohort(word, data, onset.len = 1)
}
\arguments{
\item{word}{A character string indicating the word defining the cohort.}

\item{data}{A data.frame containing CoCA lexical statistics. See \code{\link{cocaReadFreq}}.}

\item{onset.len}{The number of onset letters that must cohort members share.}
}
\value{
Returns a data.frame containing 1 row for each neighbor found in \code{data},
specifying: index (ID), wordform (w1), lemma (L1), simple wordclass (WC), CLAWS7 tag (c1), raw
CoCA frequency (coca).
}
\description{
Get lexical cohort.
}
\details{
Find all orthographic cohort members for \code{word} in data (assumes coca
data.frame). Cohort members share \code{onset.len} initial characters.
}
\author{
Dave Braze \email{davebraze@gmail.com}
}

