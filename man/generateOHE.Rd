% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/featureEngineering.R
\name{generateOHE}
\alias{generateOHE}
\title{Generate One Hot Encoding}
\usage{
generateOHE(d, features = NULL, exclude = NULL, max.levels = 100,
  convert.na = "NA", convert.empty = "Empty", remove.original = FALSE,
  full.rank = FALSE)
}
\arguments{
\item{d}{A data frame or data table containing the data set.}

\item{features}{A character vector containing a list of features to process. If left NULL, will choose ALL the factor or character fields within the data set. Can optionally use regular
expression matching to derive the list of features by prepending it with a ~ (refer to Examples).}

\item{max.levels}{An integer containing the maximum number of unique levels to consider to generate a OHE for. For example, it might not make sense to OHE a column containing 10,000 unique values for sparsity reasons.}

\item{convert.na}{A character string to impute missing character values to. Default: "NA"}

\item{convert.empty}{A character string to impute empty character values to (trim operation is applied beforehand). Default: "Empty"}

\item{remove.original}{A logical dictating whether or not we want to remove the original column that we have applied OHE on. Default: FALSE}

\item{full.rank}{A logical dictating whether or not we want to make the OHE features linearly dependent or independent. The value will depend on the type of model being built. For tree-based methods it can be valuable
to keep it FALSE, for linear models it may be better to set it to TRUE. Default: FALSE}
}
\value{
A data frame or data table containing the transformed data set with the OHE features augmented.
}
\description{
This function applies one hot encoding to factors and/or character based columns in a data frame or data table.
}
\details{
In addition, it has the ability to impute character columns that are missing or will be empty after conversion.
}
\examples{

sample.df <- data.frame(ID = floor(runif(100, 0, 10000)),
EFF_DATE = Sys.time() + runif(100, 0, 24*60*60*100),
EFF_TO = Sys.time() + runif(100, 24*60*60*100+1, 24*60*60*1000),
CUST_SEGMENT_CHR = as.character(floor(runif(100,0,10))),
STATE_NAME = ifelse(runif(100,0,1) < 0.56, 'VIC', ifelse(runif(100,0,1) < 0.44,'NSW', 'QLD')),
REVENUE = floor(rnorm(100, 500, 200)),
NUM_FEAT_1 = rnorm(100, 1000, 250),
NUM_FEAT_2 = rnorm(100, 20, 2),
NUM_FEAT_3 = floor(rnorm(100, 3, 0.5)),
NUM_FEAT_4 = floor(rnorm(100, 100, 10)),
RFM_SEGMENT = factor(x = letters[floor(runif(100,1,6))], levels = c("a","b","c","d","e")))

generateOHE(sample.df) # generate OHE on all characters or factors
generateOHE(sample.df, features = c("~CUST*", "~*SEGMENT*")) # generate only on those matching the regex
generateOHE(sample.df, features = c("~CUST*", "~*SEGMENT*"), convert.na = "Missing") # convert missing values to 'Missing' before OHE
generateOHE(sample.df, features = "~*SEGMENT*", convert.na = "Missing", full.rank = TRUE) # satisfy full rank requirements
}

