# XGBoost Bayes
xgb_bayes_fun <- function(max.depth = 6, subsample = 0.75, colsample_bytree = 0.75, eta = 0.03, alpha = 1, gamma = 0, lambda = 0, min_child_weight = 0,
                          modelparams, data) {
    params = list(max.depth = max.depth, subsample = subsample, colsample_bytree = colsample_bytree,
                  eta = eta, alpha = alpha, gamma = gamma, lambda = lambda, min_child_weight = min_child_weight, nthread = 6,
                  eval_metric = modelparams[["eval_metric"]], objective = modelparams[["objective"]])
    if (!is.null(modelparams[["num_class"]])) {
        # this is a multi-class classification
        params <- c(params, num_class = modelparams[["num_class"]])
    }
    xgb_model <- xgb.train(params = params, data = data, nround = modelparams[["nrounds"]], maximise = modelparams[["maximise"]],
                           watchlist = modelparams[["watchlist"]], early.stop.round = modelparams[["nrounds"]] * modelparams[["earlyStopRatio"]],
                           print.every.n = 5, verbose =T)
    list(Score = ifelse(modelparams[["maximise"]], 1, -1) * xgb_model$bestScore, Pred = xgb_model$nround, Model = xgb_model)
}

#' Based on the code from rBayesianOptimisation
optimiseHyperparameters <- function (FUN, data, modelparams, bounds, init_points, n_iter, acq = "ucb", kappa = 2.576, eps = 0, verbose = TRUE, ...) {
    DT_bounds <- data.table(Parameter = names(bounds), Lower = sapply(bounds, magrittr::extract2, 1), Upper = sapply(bounds, magrittr::extract2, 2), Type = sapply(bounds, class))
    DT_history <- data.table(matrix(-Inf, nrow = init_points + n_iter, ncol = length(bounds) + 2)) %>% setnames(., old = names(.), new = c("Round", names(bounds), "Value"))
    Pred_list <- vector(mode = "list", length = init_points + n_iter)
    Model_list <- list()
    message("Calculating initial points...")
    for (i in 1:init_points) {
        Sys.sleep(time = 1)
        set.seed(as.numeric(Sys.time()))
        This_Par <- Matrix_runif(n = 1, lower = DT_bounds[, Lower], upper = DT_bounds[, Upper]) %>% as.vector(.) %>%
            magrittr::inset(., DT_bounds[, Type] == "integer", round(magrittr::extract(., DT_bounds[, Type] == "integer"))) %>% magrittr::set_names(., DT_bounds[, Parameter])
        This_Par_Mod <- c(This_Par, list(data=data, modelparams = modelparams))
        This_Log <- utils::capture.output({
            This_Time <- system.time({
                This_Score_Pred <- do.call(what = FUN, args = as.list(This_Par_Mod))
                Model_list[[i]] <- This_Score_Pred$Model
            })
        })
        data.table::set(DT_history, i = as.integer(i), j = names(DT_history), value = as.list(c(Round = i, This_Par, Value = This_Score_Pred$Score)))
        Pred_list[[i]] <- This_Score_Pred$Pred
        if (verbose == TRUE) {
            paste(c("elapsed", names(DT_history)), c(format(This_Time["elapsed"], trim = FALSE, digits = 0, nsmall = 2), format(DT_history[i, "Round", with = FALSE], trim = FALSE, digits = 0,
                                                                                                                                nsmall = 0), format(DT_history[i, -"Round", with = FALSE], trim = FALSE, digits = 0, nsmall = 4)), sep = " = ", collapse = "\t") %>% cat(., "\n")
        }
    }
    message("Applying bayesian optimisation...")
    for (j in (init_points + 1):(init_points + n_iter)) {
        Par_Mat <- Min_Max_Scale_Mat(as.matrix(DT_history[1:(j - 1), DT_bounds[, Parameter], with = FALSE]), lower = DT_bounds[, Lower], upper = DT_bounds[, Upper])
        Rounds_Unique <- setdiff(1:(j - 1), which(duplicated(Par_Mat) == TRUE))
        Value_Vec <- DT_history[1:(j - 1), Value]
        GP <- GPfit::GP_fit(X = Par_Mat[Rounds_Unique, ], Y = Value_Vec[Rounds_Unique], ...)
        Next_Par <- Utility_Max(DT_bounds, GP, acq = acq, y_max = max(DT_history[, Value]), kappa = kappa, eps = eps) %>%
            Min_Max_Inverse_Scale_Vec(., lower = DT_bounds[, Lower], upper = DT_bounds[, Upper]) %>%
            magrittr::inset(., DT_bounds[, Type] == "integer", round(magrittr::extract(., DT_bounds[, Type] == "integer"))) %>% magrittr::set_names(., DT_bounds[, Parameter])

        Next_Par_Mod <- c(Next_Par, list(data=data, modelparams = modelparams))
        This_Log <- utils::capture.output({
            This_Time <- system.time({
                Next_Score_Pred <- do.call(what = FUN, args = as.list(Next_Par_Mod))
                Model_list[[j]] <- This_Score_Pred$Model
            })
        })
        data.table::set(DT_history, i = as.integer(j), j = names(DT_history), value = as.list(c(Round = j, Next_Par, Value = Next_Score_Pred$Score)))
        Pred_list[[j]] <- Next_Score_Pred$Pred
        if (verbose == TRUE) {
            paste(c("elapsed", names(DT_history)), c(format(This_Time["elapsed"], trim = FALSE, digits = 0, nsmall = 2), format(DT_history[j, "Round", with = FALSE], trim = FALSE, digits = 0,
                                                                                                                                nsmall = 0), format(DT_history[j, -"Round", with = FALSE], trim = FALSE, digits = 0, nsmall = 4)), sep = " = ", collapse = "\t") %>% cat(., "\n")
        }
    }
    Best_Par <- as.numeric(DT_history[which.max(Value), DT_bounds[, Parameter], with = FALSE]) %>% magrittr::set_names(., DT_bounds[, Parameter])
    Best_Value <- max(DT_history[, Value])
    Pred_DT <- data.table::as.data.table(Pred_list)
    Result <- list(Best_Par = Best_Par, Best_Value = Best_Value, History = DT_history, Pred = Pred_DT, Models = Model_list, Best_Round = DT_history[which.max(Value), "Round", with = FALSE])
    cat("\n Best Parameters Found: \n")
    paste(names(DT_history), c(format(DT_history[which.max(Value), "Round", with = FALSE], trim = FALSE, digits = 0, nsmall = 0),
                               format(DT_history[which.max(Value), -"Round", with = FALSE], trim = FALSE, digits = 0, nsmall = 4)), sep = " = ", collapse = "\t") %>% cat(., "\n")
    return(Result)
}

#' Train an eXtreme Gradient Boosting model [Alpha]
#'
#' @param data The dataset to train the xgboost model on. Should be a list containing training, validation and test sets (if applicable).
#' @param response The type of xgboost model to train - currently supports either 'r' (regression) or 'c' (classification).
#' @param dataConfig A list containing the data preprocessing configuration settings. Refer to details for more information.
#' @param paramConfig A list containing the bounds of the xgboost model to optimise the parameters for.
#' @param nrounds The number of rounds to run xgboost for. This should be set to a higher number if eta is fairly low.
#' @param earlyStopRatio The early stopping condition, where xgboost will stop training if it fails to find a better validation score for x of number of rounds.
#' @param train.proportion The splitting ratio for train/validation. Note that this is not stratified (future enhancement).
#' @param objective The objective for the xgboost model. Leave NULL for automatic determination. Default: NULL.
#' @param eval_metric The evaluation metric for the xgboost model. Leave NULL for automatic determination. Default: NULL.
#' @param onlyDataInd A logical containing whether or not to build the model or stop after data preprocessing. Can be used to derive the test dataset using the same transformation as the training dataset. Default: FALSE.
#' @param opt.iter The number of Bayesian optimisation iterations to perform. Default: 10.
#' @param opt.initialpts The initial number of points to initialise the Bayesian optimisation for. Default: 10.
#' @param opt.savemodel A logical containing whether or not to save the models being trialed in the Bayesian optimisation process. Default: TRUE.
#'
#' @return A list containing two elements. Models which contain all the xgb boost models built as part of the hyperparameter optimisation. Results which contain the Bayesian optimisation output.
#'
#' @export
xgboostModel_alpha <- function(data, response, modelType = "r", dataConfig = list(), paramConfig = list(), nrounds = 250, earlyStopRatio = 0.25, objective = NULL, eval_metric = NULL,
                               onlyDataInd = FALSE, opt.bayesian = TRUE, opt.iter = 10, opt.initialpts = 10, opt.savemodel = TRUE, buildEnsemble = TRUE) {
    # Convert to data.table
    require(data.table)
    lapply(data, setDT)
    dataAttrs <- c("trainData", "testData", "evalData")
    dataset <- list()

    for (attr in dataAttrs) {
        if (!is.null(data[[attr]])) {
            dataset[[attr]] <- data[[attr]]
        } else {
            dataset[[attr]] <- NULL
        }
    }

    if (is.null(dataset[["trainData"]])) {
        stop("Cannot have an empty training set.")
    }

    # Default settings
    dataConfigDef <- list(  oneHotEncoding = FALSE,  # if desired put in the data type (i.e. factor) to push for OHE
                            oneHotEncodingMaxLevels = 100,
                            oneHotEncodingClass = c("factor", "character"),
                            convertNumeric = TRUE,
                            convertNumericClass = c("factor", "character"),
                            numericProcessing = FALSE,
                            numericProcessingMethod = c("center", "scale", "impute"),
                            numericImputeMethod = c("value"),
                            numericImputeValue = -1,
                            extractDate = c("hour", "wday", "week", "month", "year", "season", "partofday"))

    # If the dataConfig is NULL, then do not perform data preprocessing!
    dataPre <- TRUE
    if (is.null(dataConfig)) {
        dataPre <- FALSE
    } else {
        # overwrite what was supplied
        dataConfig <- modifyList(dataConfigDef, dataConfig)
    }

    # Default parameter configuration settings
    paramConfigDef <- list(max.depth = c(3L, 12L),
                           subsample = c(0.5, 0.9),
                           colsample_bytree = c(0.5, 0.9),
                           eta = c(0.1, 0.3),
                           alpha = c(0, 2),
                           gamma = c(0, 1),
                           lambda = c(0, 1),
                           min_child_weight = c(0L, 10L))

    # If the paramConfig is NULL, then do not perform hyperparameter optimisation!
    hyperPar <- TRUE
    if (is.null(paramConfig)) {
        hyperPar <- FALSE
    } else {
        paramConfig <- modifyList(paramConfigDef, paramConfig)
    }

    if (dataPre) {
        message("Commencing data processing...")

        # Step 1: extract dates (and remove original)
        if (!is.null(dataConfig[["extractDate"]])) {
            for (d in dataAttrs) {
                if (is.null(dataset[[d]])) next
                suppressWarnings(dataset[[d]] <- extractDateFeatures(dataset[[d]], decompose = dataConfig[["extractDate"]], remove.original = TRUE))
            }
        }

        # Step 2: one hot encoding
        if (!is.null(dataConfig[["oneHotEncoding"]]) && dataConfig[["oneHotEncoding"]]) {
            for (d in dataAttrs) {
                if (is.null(dataset[[d]])) next
                suppressWarnings(dataset[[d]] <- generateOHE(dataset[[d]], rsClass(dataset[[d]], dataConfig[["oneHotEncodingClass"]]),
                                                             max.levels = dataConfig[["oneHotEncodingMaxLevels"]], convert.na = "MISS",
                                                             convert.empty = "EMPTY", remove.original = TRUE, full.rank = FALSE))
            }
        }

        # Step 3: Convert to numeric
        if (!is.null(dataConfig[["convertNumeric"]]) && dataConfig[["convertNumeric"]]) {
            for (d in dataAttrs) {
                if (is.null(dataset[[d]])) next

                suppressWarnings(dataset[[d]] <- convertFactorToNumeric(dataset[[d]], rsClass(dataset[[d]], dataConfig[["convertNumericClass"]]), suffix = ""))
            }
        }

        # Step 4: Numeric processing
        if (!is.null(dataConfig[["numericProcessing"]]) && dataConfig[["numericProcessing"]]) {
            for (d in dataAttrs) {
                if (is.null(dataset[[d]])) next
                suppressWarnings(dataset[[d]] <- cleanNumericFeatures(dataset[[d]], num.transform = dataConfig[["numericProcessingMethod"]],
                                                                      num.impute.method = dataConfig[["numericImputeMethod"]],
                                                                      num.impute = dataConfig[["numericImputeValue"]]))
            }
        }

        message("Finished data processing.")
    } else {
        message("Data processing was omitted.")
    }
    message("Determining model parameters...")

    # how about validation set? how does it tie in with a 'test' set? return back the (processed) dataset! + models!
    resp <- dataset[["trainData"]][, .SD, .SDcols = response][[response]]
    resp_eval <- dataset[["evalData"]][, .SD, .SDcols = response][[response]]
    num_unique_resp <- length(unique(resp))

    objective_tmp <- ""
    eval_metric_tmp <- ""
    # Step 5: Basic diagnostic check on the response variable
    if (modelType %like% 'c') {
        if (num_unique_resp == 2) {
            # binary classification
            # check if there is a need to convert to 0,1
            objective_tmp <- "binary:logistic"
            eval_metric_tmp <- "auc"

            if (!(all(resp %in% c(0,1)))) {
                # need to convert (assume not an ordinal factor)
                resp <- as.numeric(factor(resp)) - 1
                dataset[["trainData"]][, response := resp, with = F]

                resp_eval <- as.numeric(factor(resp_eval)) - 1
                dataset[["evalData"]][, response := resp_eval, with = F]
            }
        } else if (num_unique_resp > 2) {
            # multi-class classification
            objective_tmp <- "multi:softmax"
            eval_metric_tmp <- "mlogloss"

            if (!(all(resp %in% 0:(num_unique_resp - 1)))) {
                # need to convert (assume not an ordinal factor)
                resp <- as.numeric(factor(resp)) - 1
                dataset[["trainData"]][, response := resp, with = F]

                resp_eval <- as.numeric(factor(resp_eval)) - 1
                dataset[["evalData"]][, response := resp_eval, with = F]
            }
        } else {
            # error
            stop("Classification models must contain at least 2 unique response/target values.")
        }
    } else {
        if (num_unique_resp < 10) {
            warning("Attempting to build regression model on a response value that contains less than 10 unique values.")
        }
        # regression
        objective_tmp <- "reg:linear"
        eval_metric_tmp <- "rmse"
    }

    # If the user has specified the objective and/or eval metric - do not alter it
    if (is.null(objective)) {
        objective <- objective_tmp
    }
    if (is.null(eval_metric)) {
        eval_metric <- eval_metric_tmp
    }

    # constructs xgb.DMatrix from data and response
    xgbPrepareData <- function(data, response = NULL) {
        if (is.null(data)) {
            # invalid data set
            return (NULL)
        }
        if (is.null(response)) {
            # this is test data (does not have label)
            return (xgb.DMatrix(data = as.matrix(data)))
        }
        data <- data.table(data) # create a copy
        y <- data[, response, with = F]
        x <- data[, response := NULL, with = F]
        return (xgb.DMatrix(data = as.matrix(x), label = as.matrix(y)))
    }

    # convert them to xgb.DMatrix for xgboost input
    trainList <- xgbPrepareData(dataset[["trainData"]], response)
    evalList <- xgbPrepareData(dataset[["evalData"]], response)
    testList <- xgbPrepareData(dataset[["testData"]])

    # We are only interested in transforming the data into xgb.DMatrix
    if (onlyDataInd) {
        return (list(train = trainList, eval = evalList, test = testList))
    }

    # set up an evaluation set
    watchlist <- list(valid = evalList)

    # do we wish to maximise the objective?
    # Maximise: auc, map,
    # Minimise: RMSE, error, merror, logloss, mlogloss, mae
    if (tolower(eval_metric) %in% c("auc", "map")) {
        maximise <- TRUE
    } else {
        maximise <- FALSE
    }

    # Bayesian optimisation
    modelparams = list(eval_metric=eval_metric, objective = objective, maximise = maximise,
                       earlyStopRatio = earlyStopRatio, nrounds = nrounds, watchlist = watchlist)
    if (num_unique_resp > 2 && modelType %like% 'c') {
        modelparams[["num_class"]] <- num_unique_resp
    }
    message("Optimising using hyperparameters...")
    opt_result <- optimiseHyperparameters(xgb_bayes_fun, data = trainList, modelparams = modelparams, bounds = paramConfig,
                                          init_points = opt.initialpts, n_iter = opt.iter, acq = "ucb", kappa = 2.576, eps = 0, verbose = TRUE)

    message("Completed optimisation of hyperparameters.")
    # Return the model list with the optimised hyperparameter results
    return (list(train = trainList, eval = evalList, test = testList, result = opt_result, features = colnames(dataset[[attr]])))
}

autoDetermineModelType <- function(data, response) {
    # if modelType is auto(detect), then check to see if there are numeric values
    num_distinct <- length(unique(data[[response]]))
    all_num_val <- FALSE
    len_response <- length(data[[response]])
    if (suppressWarnings(sum(!is.na((as.numeric(data[[response]])))) == length(data[[response]]))) {
        # can be regression
        all_num_val <- TRUE
    }
    message(paste0("Detected ", num_distinct, " distinct target values."))
    if (num_distinct < 15) {
        # classification (even if real-valued response)
        modelType = 'c'
    } else {
        if (all_num_val) {
            # regression
            modelType = 'r'
        } else {
            # multi-class classification
            modelType = 'c'
        }
    }
    return(modelType)
}



#> xgb.importance(iris.xgb[["features"]],model = iris.xgb[["result"]][["Models"]][[2]])
#' Easy to use version of xgboost
#'
#' Assumes default parameters for data configuration. Designed to hide complexity.
#'
buildModel <- function(data, response, modelType = "auto", calculationBudget = "balanced",
                       trainProportion = 0.8, evalProportion = 0.2, onlyDataInd = FALSE) {
    if (modelType == "auto") {
        modelType <- autoDetermineModelType(data, response)
    }
    nrounds = 500
    if (tolower(calculationBudget) == "speed") {
        opt.iter = 2
        opt.initialpts = 10
        nrounds = 500
        params <- list(max.depth = c(3L, 7L),
                       subsample = c(0.5, 0.75),
                       colsample_bytree = c(0.5, 0.75),
                       eta = c(0.1, 0.3),
                       alpha = c(0, 2),
                       gamma = c(0, 2),
                       lambda = c(0, 2),
                       min_child_weight = c(0L, 3L))
    } else if (tolower(calculationBudget) == "accuracy") {
        opt.iter = 20
        opt.initialpts = 30
        nrounds = 2000
        params <- list(max.depth = c(3L, 12L),
                       subsample = c(0.5, 0.9),
                       colsample_bytree = c(0.5, 0.9),
                       eta = c(0.01, 0.05),
                       alpha = c(0, 3),
                       gamma = c(0, 3),
                       lambda = c(0, 3),
                       min_child_weight = c(0L, 6L))
    } else {
        # catch all
        opt.iter = 8
        opt.initialpts = 15
        nrounds = 1000
        params <- list(max.depth = c(3L, 9L),
                       subsample = c(0.5, 0.75),
                       colsample_bytree = c(0.5, 0.75),
                       eta = c(0.05, 0.2),
                       alpha = c(0, 2),
                       gamma = c(0, 2),
                       lambda = c(0, 1),
                       min_child_weight = c(0L, 3L))
    }
    message(paste0("Building a ", ifelse(modelType == 'r','regression','classification'), " model..."))
    # split the data first
    if (modelType %like% 'r') {
        dataSet <- splitDataToTrainTestDataFrame(data, trainProportion = trainProportion, evalProportion = evalProportion)
    } else {
        dataSet <- splitDataToTrainTestStratified(data, strataCols = response, trainProportion = trainProportion,
                                                  evalProportion = evalProportion)
    }

    model <- xgboostModel(data = dataSet, response = response, onlyDataInd = onlyDataInd, paramConfig = params, modelType = modelType, nrounds = nrounds, opt.iter = opt.iter, opt.initialpts = opt.initialpts)
    return (model)
}

buildScoreData <- function(data, response) {
    return (buildModel(data, response, trainProportion = 0.05, onlyDataInd = TRUE))
}


############ TO DO FUNCTIONS ############
# if label supplied, performance statistics
# best, avg
scoreModel <- function(model, data, method = "best") {
    if (tolower(method) == "best") {
        # Choose the best model from the model list
        bestRound <- model[["result"]]$Best_Round$Round
        xgbModel <- model[["result"]]$Models[[bestRound]]
        pred <- predict(xgbModel, data$test)
    } else if (tolower(method) == "avg") {
        # Average them all
        pred <- NULL
        for (m in model[["result"]]$Models) {
            print(str(m))
            pred <- rbind(pred, predict(m, data$test))
        }
        return (sapply(data.frame(pred), mean))
    } else {
        stop("Method hasn't been implemented yet.")
    }
    return (pred)
}

# Uses the validation set to build the ensemble selection
# modelResult is a list containing a list of 'prebuilt' models (attached with data)
buildEnsembleSelection <- function(modelResult) {
    for (models in modelResult) {
        eval_data <- models[["eval"]]
        xgbModels <- models[["Models"]]
        for (ind in xgbModels) {

        }
    }
}

#' Run an eXtreme Gradient Boosting model
#'
#' @param datasets a list of train, test and validation datasets
#' @param target the name of the target feature in datasets
#' @param paramxgBoost a list of parameters for xgboost
#' @param nrounds the max number of iterations
#' @param early.stop.round If NULL, the early stopping function is not triggered. If set to an integer k, training with a validation set will stop if the performance keeps getting worse consecutively for k rounds.
#' @param verbose If 0, xgboost will stay silent. If 1, xgboost will print information of performance. If 2, xgboost will print information of both
#' @param print.every.n Print every N progress messages when verbose>0. Default is 1 which means all messages are printed.
#'
#' @examples
#' set.seed(42)
#' x = iris
#' x$Species = ifelse(x$Species == "versicolor",1, 0)
#' dat = splitDataToTrainTestDataFrame(x,.9,.25)
#' mod = xgboostModel(dat,"Species",nrounds = 200, early.stop.round = 5, verbose = 1)
#'
#' @export
xgboostModel = function(datasets,
                        target,
                        paramxgBoost = NULL,
                        nrounds= 100,
                        early.stop.round = 5,
                        print.every.n = 50,
                        verbose = 1) {
    require(xgboost)
    require(data.table)

    lapply(datasets, setDT)

    # use default tuning parameters
    if(is.null(paramxgBoost)){
        if(length(unique(datasets$trainData[[target]])) == 2 & all(unique(datasets$trainData[[target]]) %in% c(1,0))){
            paramxgBoost <- list(
                "objective"  = 'binary:logistic'
                , "eval_metric" = "auc"
                , "eta" = 0.01
                , "subsample" = 0.7
                , "colsample_bytree" = 0.5
                , "min_child_weight" =6
                , "max_depth" = 9
                , "alpha" = 4
                , "nthread" = 6
            )
        }else{
            paramxgBoost <- list(
                "objective"  = 'reg:linear'
                , "eval_metric" = "rmse"
                , "alpha" = 4
                , "lambda" = 6
                , "nthread" = 6
            )
        }
    }

    # set up the data into the right format
    datasetsxgb = convertyDataToXgBoost(datasets, target)

    # set up an evaluation set
    watchlist <- list('val' = datasetsxgb$evalData)

    # run the model
    model = xgb.train(
        nrounds = nrounds
        , params = paramxgBoost
        , data = datasetsxgb$trainData
        , early.stop.round = early.stop.round
        , watchlist = watchlist
        , print.every.n = print.every.n
        , verbose = verbose
    )

    return(
        list(
            datasetsxgb = datasetsxgb,
            model = model,
            modelConfig = paramxgBoost,
            target = target
        )
    )
}

#' Get the variable Importance of a xgboost Model
#'
#' @description
#' Generic Function to get the importance of features in a xgboost model
#'
#' @param mod a Model from \code{xgboostModel}
#' @param N The Number of features to return
#' @param allowedVariables Features that are allowed to be included
#'
#' @return A list containing a data.table with the variable importance scores and a ggplot
#'
#' @examples
#' set.seed(42)
#' x = iris
#' x$Species = ifelse(x$Species == "versicolor",1, 0)
#' dat = splitDataToTrainTestDataFrame(x,.9,.25)
#' mod = xgboostModel(dat,"Species")
#' v = xgboostVariableImportance(mod)
#'
#' v = xgboostVariableImportance(mod, 30, allowedVariables = c("Sepal.Length","Sepal.Width"))
#'
#' @export
xgboostVariableImportance = function(mod, N = 30, allowedVariables = NULL){

    imp <- xgb.importance(mod$datasetsxgb$features, model = mod$model)
    imp <- imp[order(imp$Gain, decreasing = T),]

    if(length(allowedVariables)>0) {
        imp = imp[imp$Feature %in% allowedVariables,]
    }

    imp = head(imp, n = min(N, nrow(imp)))

    pl <-  ggplot(imp, aes(x=reorder(imp$Feature, imp$Gain), y=imp$Gain)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        ggtitle(paste0("Feature Importance - ",mod$target)) +
        xlab("Features") +
        ylab("Gain")  # xgb.plot.importance(imp)

    res = list(
        importanceTable = imp,
        importancePlot = pl
    )
    return(res)
}

