% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_CenturyFootnote.R
\name{check_CenturyFootnote}
\alias{check_CenturyFootnote}
\title{Check location of century footnote}
\usage{
check_CenturyFootnote(path = ".", strict = FALSE)
}
\arguments{
\item{path}{Directory containing the \strong{aux} file. In particular, \code{pdflatex} must be run before running this function.}

\item{strict}{Stop if suspected of incorrect placement. Otherwise a note.}
}
\value{
If CenturyFootnote correctly placed, \code{NULL} invisibly. Otherwise, an error.
}
\description{
The formatting of footnote text should be redefined when there are more than 99 footnotes in the document.
}
