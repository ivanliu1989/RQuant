% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualisations.R
\name{quantColours}
\alias{quantColours}
\title{Return the Quant colours.}
\usage{
quantColours(i = NULL, inv = 1)
}
\description{
This function takes no arguments and returns a vector of 7 Hex codes
describing the colours used in the RQuant logo.
}
\examples{
\dontrun{
quantclrs <- quantColours(inv = 3)
scales::show_col(quantclrs)
}
}

