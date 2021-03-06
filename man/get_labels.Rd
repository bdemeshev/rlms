% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rlms.R
\name{get_labels}
\alias{get_labels}
\title{Get value labels of a vector}
\usage{
get_labels(x)
}
\arguments{
\item{x}{a vector}
}
\value{
character vector value labels
}
\description{
Get value labels of a vector
}
\details{
Get value labels of a vector
}
\examples{
x <- haven::labelled(c(1, 1, 2, 3, 4), c(Male = 1, Male = 2, Female = 3))
get_labels(x)
}

