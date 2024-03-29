% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xgboostFeatImp.R
\name{xgboostScoreExplain}
\alias{xgboostScoreExplain}
\title{Explain an observation's score from an xgboost model and show an shiny app}
\usage{
xgboostScoreExplain(model, modelType = c("multiClassification",
  "binaryClassification", "regression"), target = "target", dt.singleRow,
  nclasses = 3, shiny = F, sampleData = NULL, top_n_features = 10)
}
\arguments{
\item{model}{A xgboost model}

\item{modelType}{Either c("binaryClassification", "regression")}

\item{target}{The target variable}

\item{dt.singleRow}{A single row of observation}

\item{nclasses}{Number of classes if you are explaining a mulitiClassification model, default is 3}

\item{shiny}{T/F, whether or not to generate an shiny app as part of the returned object}

\item{sampleData}{The sample data which which was used to train the model}
}
\value{
a list containing summaryTrees, featureWtsIndividual, pred, featureImportanceOverall and (shiny)
}
\description{
By taking the trained xgboost model and a single observation (without the dependent variable) as the input, the function produces a summary of each tree including information such as gain, features on the path, and feature weights. In addition, the function ouputs the aggregated individual feature importance, the calculated prediction of the observation, as well as the overall feature importance.
}
\note{
The function now only supports binaryClassification model. These two limitations will be enabled in the future.
}
\examples{
require(xgboost)

# regression --------------
data(mtcars)

m.train <- data.matrix(mtcars[, -1])
dtrain <- xgb.DMatrix(data = m.train, label = mtcars$mpg, missing = NaN)

params <- list(objective = "reg:linear",
               booster = "gbtree",
               eval_metric = "rmse",
               eta                 = 0.02,
               max_depth           = 4,
               subsample           = 0.83,
               colsample_bytree    = 0.77
)

model <- xgb.train(params = params
                   , data = dtrain
                   , nrounds = 3
                   # , watchlist = watchlist
                   , maximize = F
                   , print.every.n = 1
                   # , early.stop.round = 1
)

dt.singleRow<- mtcars[1, ]
m.try <- data.matrix(dt.singleRow[, -1])
dtry <- xgb.DMatrix(data = m.try, label = dt.singleRow$mpg, missing = NaN)

res <- xgboostScoreExplain(model, modelType = "regression", target = "mpg", dt.singleRow = dt.singleRow)

# binaryClassification --------------
set.seed(42)
x = iris
x$Species = ifelse(x$Species == "versicolor",1, 0)
dat = splitDataToTrainTestDataFrame(x,.9,.25)
mod = xgboostModel(dat,"Species",nrounds = 200, early.stop.round = 5, verbose = 1)

# single row of observation
xx <- x[1, -5]

# model
model <- mod$model

m.try <- data.matrix(xx)
dtry <- xgb.DMatrix(data = m.try, label = x[1,]$Species, missing = NaN)

# score explain
res <- xgboostScoreExplain(model = model, modelType = "binaryClassification", target = "Species", dt.singleRow = xx, shiny = T, sampleData = x)
res


# multiClassification -----------------------------------------------------
require(data.table)
require(xgboost)
require(caret)
data(iris)

setDT(iris)

iris[, target := ifelse(Species == "setosa", 0
                        , ifelse(Species == "versicolor", 1, 2))]
iris[, Species := NULL]

# add some noise
set.seed(1)
iris <- rbind(iris
              , data.table(Sepal.Length = rnorm(20, mean = 4, 1)
                           , Sepal.Width = rnorm(20, mean = 2, .5)
                           , Petal.Length = rnorm(20, mean = 2, .1)
                           , Petal.Width = rnorm(20, mean = 1, .01)
                           , target = sample(0:2, 20, replace = T)
              )
)

set.seed(1)
ind.train <- createDataPartition(iris$target, p = .8, list = F)
dt.train <- iris[ind.train]
dt.valid <- iris[!ind.train]

mx.train <- as.matrix(dt.train[, !c("target"), with = F])
mx.valid <- as.matrix(dt.valid[, !c("target"), with = F])

dmx.train <- xgb.DMatrix(data = mx.train, label = dt.train$target)
dmx.valid <- xgb.DMatrix(data = mx.valid, label = dt.valid$target)

watchlist <- list(valid = dmx.valid, train = dmx.train)
params <- list(objective = "multi:softmax"
               , booster = "gbtree"
               , num_class = 3
               , eta = 1)

set.seed(1)
mod <- xgb.train(params = params
                 , data = dmx.train
                 , watchlist = watchlist
                 , nrounds = 10
                 , verbose = 1
                 , print.every.n = 1
                 , early.stop.round = 3
                 , maximize = F)

dt.try <- dt.valid[, !c("target"), with = F][1]
dt.try

res <- xgboostScoreExplain(model = mod, modelType = "multiClassification", target = "target", dt.singleRow = dt.try, nclasses = 3, shiny = F, sampleData = x)
res


}
\seealso{
\code{\link{xgboostModel}}
}

