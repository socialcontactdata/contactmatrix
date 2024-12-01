% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S3-contactmatrix.R
\name{tidy.contactmatrix}
\alias{tidy.contactmatrix}
\title{Convert a \code{contactmatrix} to a tidy \code{data.frame}}
\usage{
\method{tidy}{contactmatrix}(x, ...)
}
\arguments{
\item{x}{A \code{contactmatrix} object}

\item{...}{Ignored for now}
}
\description{
Convert a \code{contactmatrix} to a tidy \code{data.frame}
}
\examples{
cm2d <- new_contactmatrix(
  from  = c("[0,5)", "[5,10)",  "[5,10)"),
  to    = c("[0,5)", "[10,15)", "[15,20)"),
  value = c(0.32   , 0.46   , 0.72   ),
  symmetric = TRUE
)
tidy(cm2d)
}
