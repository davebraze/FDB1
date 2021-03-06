% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/A195.R
\name{read.mer}
\alias{read.mer}
\title{Read a 'merge' file (*.mer), as might be exported from filemaker.}
\usage{
read.mer(filename, columns, ...)
}
\arguments{
\item{filename}{A *.mer file, as generated by filemaker.}

\item{columns}{Columns to select from *.mer file.}

\item{...}{Additional arguments passed to \code{\link{read.csv}}.}
}
\value{
A data.frame
}
\description{
Read a 'merge' file (*.mer), as might be exported from Filemaker.
}
\details{
This is a wrapper around \code{\link{read.csv}} to simplify reading tables exported from
Filemaker as 'merge' files.
}
\author{
Jon Gordils

David Braze \email{davebraze@gmail.com}
}
\seealso{
\code{\link{read.csv}}
\code{\link{read.table}}
}

