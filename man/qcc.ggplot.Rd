% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CusumEWMA.R
\name{qcc.ggplot}
\alias{qcc.ggplot}
\title{Convert QCC Plot into GGPlot}
\usage{
qcc.ggplot(object, title = "QCC Chart for AUD/USD Prices", rawplot = FALSE,
  font_size = 8)
}
\arguments{
\item{object}{A qcc object returned from \code{qcc()} function.}

\item{title}{Title to put in the plot}

\item{rawplot}{using raw data or not}

\item{font_size}{size of font}
}
\value{
A \code{grid} object containing the complete plot.
}
\description{
Implementation of plot.qcc using ggplot2 and grid
}
\examples{
AUD_USD = getOandaInstrumentCandles(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = 'AUD_USD',
price = 'M', granularity = 'D', count = 250)
for(i in 4:7){
AUD_USD[, i] <- as.numeric(AUD_USD[, i])
}
AUD_USD_p = na.omit(diff(as.matrix(log(AUD_USD[, 4:7]))))
AUD_USD_v = as.numeric(AUD_USD$volume)
x = AUD_USD_p
x = AUD_USD_v
object = qcc(AUD_USD_v[1:200], newdata = AUD_USD_v[201:250], type = "xbar.one")
ggsave("qcc_ggplot.png", qcc.ggplot(object))

}

