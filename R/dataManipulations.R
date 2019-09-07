#' Create an empty data.frame
#'
#' Create an empty data.frame
#'
#' @param cols The column names of the data frame
#' @param N The number of rows to populate with NA
#' @return A \code{data.frame} of dimension \code{N*length(cols)}
#' @examples
#' emptyDF(c("A", "B"))
#' @export
emptyDF = function(cols, N = 0){
    res = data.frame(matrix(vector(), N, length(cols), dimnames=list(c(), cols)),
                     stringsAsFactors=F)
    return(res)
}

DFfield = function(x){
    if(is.null(x)) return(NA)
    return(x)
}


#' Merge a list of data.frames
#'
#' This function takes in a list of data sets and merges them all together as a
#' series of left joins. There are some mandatory merge fields and some optional
#' fields that we will try to merge on if they exist. We assume that the first
#' element in the list is complete and has both the optional and the mandatory
#' columns.
#'
#' @param datasets a \code{list} of data.frames with column names
#' @param mandatoryMergeFields The fields that will be used in the join
#' @param optionalMergeFields The fields that will be used in the join if they exist
#'
#' @examples
#' d1 = mtcars
#' d2 = mtcars
#' colnames(d2)[!colnames(d2)%in%c("gear","carb")] = LETTERS[1:(ncol(d2)-2)]
#' View(mergeDataFrames(list(d1, d2), c("gear", "carb")))
#' @export
mergeDataFrames = function(datasets, mandatoryMergeFields, optionalMergeFields = NULL){
    res = datasets[[1]]
    for(i in 2: length(datasets)){
        newtmp = datasets[[i]]
        newNames = names(newtmp)
        existingNames = names(res)
        # make sure all of the mandatory fields are in the data set
        if(!all(mandatoryMergeFields%in%newNames)){
            stop("data set does not all contain mandatory columns")
        }
        # only keep the fields that exist in the new data set for the merge
        mergeFields  = c(mandatoryMergeFields, optionalMergeFields[optionalMergeFields%in%newNames])

        # drop the columns that are already in the result (except for joining columns)
        allowedColumns = c(mergeFields, newNames[!newNames%in%existingNames])
        newtmp = newtmp[,newNames%in%allowedColumns]

        res = merge(res, newtmp, by = mergeFields, all.x = T)
    }
    return(res)
}


#' Extract variable types from data.frames
#'
#' This function extracts the variable classes from a \code{data.frame}.
#' @param x A \code{data.frame}
#'
#' @examples
#' getDataFrameVariableTypes(mtcars)
#'
#' @export
getDataFrameVariableTypes = function(x){
    varTypes = NULL
    for(i in 1:ncol(x)){
        varTypes = c(varTypes,class(x[,i]))
    }
    names(varTypes) = colnames(x)
    return(varTypes)
}


#' Convert a list to a data frame
#'
#' This function takes in a list and converts it to a \code{data.frame}.
#' @param x A \code{list} where all of the elements represents a row and are
#' typically are of exqual length
#'
#' @examples
#' as.data.frame.list(list(x = rnorm(100), y = rnorm(100)))
#'
#' @export
as.data.frame.list <- function(x, row.names=NULL, optional=FALSE, ...) {
    if(!all(unlist(lapply(x, class)) %in%
            c('raw','character','complex','numeric','integer','logical'))) {
        warning('All elements of the list must be a vector.')
        NextMethod(x, row.names=row.names, optional=optional, ...)
    }
    allequal <- all(unlist(lapply(x, length)) == length(x[[1]]))
    havenames <- all(unlist(lapply(x, FUN=function(x) !is.null(names(x)))))
    if(havenames) { #All the vectors in the list have names we can use
        colnames <- unique(unlist(lapply(x, names)))
        df <- data.frame(matrix(
            unlist(lapply(x, FUN=function(x) { x[colnames] })),
            nrow=length(x), byrow=TRUE))
        names(df) <- colnames
    } else if(allequal) { #No names, but are of the same length
        df <- data.frame(matrix(unlist(x), nrow=length(x), byrow=TRUE), ...)
        hasnames <- which(unlist(lapply(x, FUN=function(x) !is.null(names(x)))))
        if(length(hasnames) > 0) { #We'll use the first element that has names
            names(df) <- names(x[[ hasnames[1] ]])
        }
    } else { #No names and different lengths, we'll make our best guess here!
        warning(paste("The length of vectors are not the same and do not ",
                      "are not named, the results may not be correct.", sep=''))
        #Find the largest
        lsizes <- unlist(lapply(x, length))
        start <- which(lsizes == max(lsizes))[1]
        df <- x[[start]]
        for(i in (1:length(x))[-start]) {
            y <- x[[i]]
            if(length(y) < length(x[[start]])) {
                y <- c(y, rep(NA, length(x[[start]]) - length(y)))
            }
            if(i < start) {
                df <- rbind(y, df)
            } else {
                df <- rbind(df, y)
            }
        }
        df <- as.data.frame(df, row.names=1:length(x))
        names(df) <- paste('Col', 1:ncol(df), sep='')
    }
    if(missing(row.names)) {
        row.names(df) <- names(x)
    } else {
        row.names(df) <- row.names
    }
    return(df)
}


#' Append a data set to another data set
#'
#' This function takes two data sets and appends the second onto the first.
#' The Second may not have all of the columns as the firstin a list and converts it to a \code{data.frame}.
#' @param full A \code{data.frame} that will be the base
#' @param tmp A \code{data.frame} that will be joined onto the bottom of \code{full}
#'
#' @examples
#' x = data.frame(A = rnorm(3), B = rnorm(3), C = rnorm(3))
#' y = data.frame(A = rnorm(3), C = rnorm(3))
#' appendToTable(x, y)
#'
#' @export
appendToTable = function(full, tmp){
    cols = colnames(full)
    base = data.frame(matrix(vector(), nrow(tmp), length(cols),dimnames=list(c(), cols)),stringsAsFactors=F)
    for(n in cols){
        if(!is.null(tmp[[n]])){
            base[[n]] = tmp[[n]]
        }
    }
    res = rbind(full, base)
    return(res)
}


#' Summary Dataframe
#'
#' @description
#' This function takes a data frame as an input, and provides a quick summary of the contents of the data frame.
#' It provides table-level (size and last date updated) and column-level (e.g. minimum, maximum, missing values) information
#' For character fields or factors, it provides the number of distinct values. For continuous values, it shows the minimum and maximum.
#' In addition it returns an example value, the number of missing values, the technical and statitical data type
#'
#' @param df The data frame to be summarised
#'
#' @return The function returns a list. The list entries contain tabel-level and column-level results
#'
#' @seealso \code{\link{str}}, \code{\link{head}} \code{\link{summary.data.frame}}
#'
#' @examples
#' res <- summarise.data.frame(mtcars)
#' res$table_level
#' res$column_level
#' print(res)
#'
#' @export
summarise.data.frame <- function(df){
    stat_data_type <- unlist(lapply(df, function(y) ifelse((is.integer(y) & length(unique(y)) > 5) | is.double(y), "continuous","categorical")))
    tech_data_type <- unlist(lapply(df, class))
    min <- unlist(lapply(df, function(y) ifelse((is.integer(y) & length(unique(y)) > 5) | is.double(y), min(y, na.rm = TRUE), NA)))
    max <- unlist(lapply(df, function(y) ifelse((is.integer(y) & length(unique(y)) > 5) | is.double(y), max(y, na.rm = TRUE), NA)))
    missing <- apply(df, 2, function(y) sum(length(which(is.na(y)))))
    distinct <- apply(df, 2, function(y) length(unique(y)))
    has_num_vals <- unlist(lapply(df, function(y) ifelse(sum(is.na(as.numeric(y))==FALSE) > 0, "Y", "N")))
    row_count <- nrow(df)
    example <- apply(df, 2, function(y) y[sample.int(row_count, size=1)])
    result_cols <- data.frame(min, max, missing, distinct, has_num_vals, stat_data_type, tech_data_type, example)

    last_date_changed <- as.character(Sys.Date())
    result_tbl <- data.frame(row_count, last_date_changed)

    lst <- list(
        table_level = result_tbl
        , column_level = result_cols
    )

    return(lst)

}


#' Summary Dataframe
#'
#' @description
#' This function takes a data frame as an input, and provides a quick summary of the contents of the data frame.
#'
#' For character fields or factors, it provides a frequency table, sorted decreasing, if there are relatively few
#' distinct values.  Otherwise, the first few values are shown.
#'
#' For numeric fields, including date or datetime fields, it provides quartiles.
#'
#' It also provides a pairs plots for the first few numeric fields.
#'
#' @param df The data frame to be summarised
#' @param filename A character string to use as the name of the output files
#' @param round.date A logical, whether to round datetime values to just date
#' @param n.values Integer, the maximum number of distinct values to show for frequency tables.  For fields with more than
#'  this number of distinct values, a frequency table will be displayed.  For fields with less than this number of
#'  distinct values, the first few values will be displayed.
#' @param n.print Integer, the number of sample values to display.
#' @param n.pairs Integer, the number of numeric variables to display in the pairs plot.
#'
#' @return The function does not return any value.  But it does produce a text file and a png file, based on the supplied
#'  filename stub.
#'
#' @seealso \code{\link{str}}, \code{\link{summary.data.frame}}, \code{\link{head}}
#'
#' @examples summarise.df(mtcars)
#'
#' @export
summarise.df <- function( df, filename='output',round.date=TRUE, n.values=10, n.print=8, n.pairs=12) {
    library(data.table)
    if(!is.data.frame(df)) {
        stop('Argument is not a data frame.  This function only works with data frames.\n')
    }
    setDT(df)
    # constants ---------------------------------------------------------------

    col.names <- names(df)
    max.length<-max(nchar(col.names))
    fixed.names<-format(col.names, width = max.length)
    my.modes<-sapply(df,mode)
    my.classes<-sapply(df, function(x) class(x)[1])
    numeric.cols <- col.names[my.classes == 'numeric']
    date.cols <- col.names[my.classes == 'POSIXct']

    # output --------------------------------------------------------

    sink(file=paste(filename,'.txt',sep=''))

    # cat('______________________________________________________\n')
    cat('Data frame:', deparse(substitute(df)), '\n')
    cat('Dimensions:', dim(df), '\n')
    # cat("Numeric fields: ", length(numeric.cols))
    cat('Field classes:\n')
    x<-tabulate(as.factor(my.classes))
    names(x)<-unique(my.classes)
    print(x)
    # print(table(my.classes))
    flush.console()

    if(length(numeric.cols) > 1) {
        pairs(head(df[,head(numeric.cols, n.pairs), with = F],1000))
        dev.copy(png, filename = paste(filename,'.png',sep=''))
        dev.off()
    }

    for (i in  seq(ncol(df))){
        cat('\n$'
            ,fixed.names[i]
            ,' : '
            ,my.classes[i]
            # ,'\n'
            ,sep='')
        flush.console()

        if(my.classes[i] == 'numeric') {
            if(length(unique(df[[i]])) <= n.values) {
                # small number of unique values, so print start of frequency table
                x<-table(df[[i]],useNA='ifany')
                x<-sort(x, decr=TRUE)
                cat(" : frequency\n")
                print(head(x,n.print))
            }
            else {
                # large number of unique values, so print quartiles
                x<-quantile(df[[i]],na.rm=TRUE)
                cat(' : quartiles\n')
                print(x)
            }
        }

        if(my.classes[i] == 'character'){
            if(length(unique(df[[i]])) <= n.values) {
                # small number of unique values, so print start of frequency table
                x<-table(df[[i]],useNA='always')
                x<-sort(x, decr=TRUE)
                cat(": frequency")
                print(head(x,n.print))
            }
            else {
                # large number of unique values, so print first few
                cat(": sample\n")
                x<-df[[i]]
                print(head(x,n.print))
            }
        }

        if(my.classes[i] == 'POSIXct') {
            if(length(unique(df[[i]])) <= n.values) {
                # small number of unique values, so print start of frequency table
                x<-table(df[[i]],useNA='ifany')
                x<-sort(x, decr=TRUE)
                cat(" : frequency")
                print(head(x,n.print))
            }
            else {
                # large number of unique values, so print quartiles
                x<-quantile(df[[i]],na.rm=TRUE)
                cat(' : quartiles\n')
                print(strftime(x, '%d%b%Y', usetz=FALSE))
            }
        }
    }

    sink()

    cat('Successful completion.\n')
}



#' Rough conversion of data set to numeric data set
#'
#' @export
roughModelDataSetToNumeric = function (modellingData){
    features = names(modellingData)
    # quickly make everthing numeric`
    for(feature in features){
        if (class(modellingData[[feature]])%in%c("character", "factor")) {
            lev <- unique(unlist(modellingData[[feature]]))
            modellingData[[feature]] <- as.integer(factor(modellingData[[feature]], levels=lev))
        }
    }
    #replaces missing
    modellingData[is.na(modellingData)] = -999
    modellingData = as.data.frame(modellingData)

    return(modellingData)
}

#' function to take in a set of ids and produce a list of training
#' and test IDs using random sampling
#' @export
getTrainTestIds = function(x, trainProportion, evalProportion = NULL, seed = 999, verbose = F){
    # for reproducability
    set.seed(seed)
    # get the number of cases in the full data set
    N = length(unique(x))
    trainN = N * (trainProportion)
    trainId = sample(1:N, floor(trainN))
    testId = (1:N)[! 1:N %in% trainId]
    #creat an evaluation set from within the training set
    if(is.null(evalProportion)){
        evalId = NULL
    }else{
        evalN = trainN * (evalProportion)
        evalId = sample(trainId, floor(evalN))
        trainId = trainId[!trainId%in%evalId]
    }
    # put everthing into a list to be returned
    res = list(
        trainId = trainId,
        testId = testId,
        evalId = evalId
    )
    if(verbose){
        cat(  "Training Cases:     ",length(trainId),
              "\nTest Cases:         ",length(testId),
              "\nEvaluation Cases:   ",length(evalId))
    }
    gc()
    return(res)
}


#' Function to split a dataset and return training, test and eval sets as dataframes
#' @export
splitDataToTrainTestDataFrame = function(x, trainProportion, evalProportion = NULL, seed = 999, verbose = F){
    # get the training/test IDs
    ids = getTrainTestIds(1:nrow(x), trainProportion, evalProportion, seed, verbose)
    if(verbose){
        cat("\nGenerating datasets\n")
    }
    # put everthing into a list to be returned
    res = list(
        trainData = x[ids$trainId, ],
        testData = x[ids$testId, ],
        evalData = x[ids$evalId, ]
    )
    gc()
    return(res)
}



#' Function to split a dataset and return training, test and eval sets as dataframes
#' @export
splitDataToTrainTestDataFrameFromId = function(x, IdVar, trainProportion, evalProportion = NULL, seed = 999, verbose = F){
    # get the training/test IDs
    if(!IdVar%in%names(x)){
        stop("variable ", IdVar, " not in data set")
    }
    uniqueIds = as.character(unique(x[[IdVar]]))
    ids = getTrainTestIds(uniqueIds, trainProportion, evalProportion, seed, verbose)
    if(verbose){
        cat("\nGenerating datasets\n")
    }
    # put everthing into a list to be returned
    res = list(
        trainData = x[x[[IdVar]]%in%uniqueIds[ids$trainId], ],
        testData = x[x[[IdVar]]%in%uniqueIds[ids$testId], ],
        evalData = x[x[[IdVar]]%in%uniqueIds[ids$evalId], ]
    )
    gc()
    return(res)
}

#'Convert a training dataset (including target variable) into a list
#'of xgb.DMatrix objects.
#'
#'Converts training dataset into xgboost datasets by
#'using \code{\link[xgboost]{xgb.DMatrix}}.
#'
#'@param x A list containing an element named 'trainData'. \code{x$trainData}
#'must contain the target variable
#'@param target A character string, the name of the target variable column
#'@examples
#'DF1 <- data.frame(y = 1:6, a = letters[1:2], b = gl(3,2,labels = c("H","M","L")))
#'train.list <- list(trainData = DF1)
#'convertyDataToXgBoost(train.list, "y")
#' @export
convertyDataToXgBoost = function(x, target){
    require(xgboost)
    require(data.table)

    if(!target%in%names(x$trainData)){
        stop("A valid target variable name must be provided")
    }
    features = names(x$trainData)[!names(x$trainData)%in%target]

    res = lapply(x, function(y){
        setDT(y)
        if(nrow(y)>0){
            tmptarget = y[, target, with = F][[1]]
            if(is.factor(tmptarget)){
                lev <- unique(unlist(tmptarget))
                tmptarget <- as.integer(factor(tmptarget, levels=lev)) - 1
            }
            tmp = xgb.DMatrix(data.matrix(y[, features, with = F]),
                              label = tmptarget,
                              missing = NA)
            return(tmp)
        }else{
            return(NULL)
        }
    })

    res[["targetTest"]] = x$testData[, target, with = F]
    res[["features"]] = features
    gc()
    return(res)
}



#' Function to do stratified sampling for model data
#'
#' Function to stratified split a dataset and return training, test and eval sets as data.frame or data.table
#'
#' @param x A data.frame or data.table object containing all data to be split
#' @param strataCols Names of columns or features that will be used to do stratification
#' @param trainProportion Proportion of training set
#' @param evalProportion Proportion of validation set
#' @param levels Threshold of determine a feature to be categorical and qualified for stratification
#' @param seed Random seeds for reproducibility
#' @param DT If the function returns a list of data.tables or data.frames
#'
#' @return A \code{list} of \code{data.table} or \code{data.frame}
#' @examples
#' data(mtcars)
#' res <- splitDataToTrainTestStratified(mtcars, names(mtcars), 0.7, 0.3, 15, 999, T)
#' res$trainData
#' res$testData
#' res$evalData
#'
#' @export
splitDataToTrainTestStratified <- function(x, strataCols = NULL, trainProportion, evalProportion = NULL,
                                           levels = 50, seed = 999, DT = T) {
    # for reproducibility
    set.seed(seed)
    library(data.table)
    setDT(x)
    # main
    if(is.null(strataCols)){
        message('No strata features provided, will be proceeding with normal sampling')
        res <- splitDataToTrainTestDataFrame(x, trainProportion, evalProportion, seed, F)
    }else{
        # check if there is wrong feature names
        if(!all(strataCols%in%names(x))) stop("some variables provided not in data set")

        # filter possible continuous features
        filterCols <- unlist(lapply(strataCols, function(c) nrow(unique(x[,c, with = F]))))
        filterCols <- strataCols[filterCols<=levels]
        finCols <- strataCols[strataCols %in% filterCols]
        if(length(finCols)==0){
            stop("no variable left after removing continuous varirables, please update and rerun...")
        }

        # start sampling for train / test
        if(trainProportion < 1 & trainProportion > 0){
            df.table <- x[, .N, by = finCols]
            n <- df.table[, ss := round(N * trainProportion, digits = 0)]
            setkeyv(x, finCols)
            setkeyv(n, finCols)

            x[, .RNID := sequence(nrow(x))]
            trainData <- x[.RNID %in% x[n, list(.RNID = sample(.RNID, ss, replace = F)), by = .EACHI]$`.RNID`]
            testData <- x[!.RNID %in% trainData$`.RNID`]
            trainData[, .RNID := NULL]
            testData[, .RNID := NULL]
        }else{
            trainData <- x
            testData <- NULL
        }

        # start sampling for validation
        if(!is.null(evalProportion)){
            x <- trainData
            df.table <- x[, .N, by = finCols]
            n <- df.table[, ss := round(N * (1-evalProportion), digits = 0)]
            setkeyv(x, finCols)
            setkeyv(n, finCols)

            x[, .RNID := sequence(nrow(x))]
            trainData <- x[.RNID %in% x[n, list(.RNID = sample(.RNID, ss, replace = F)), by = .EACHI]$`.RNID`]
            evalData <- x[!.RNID %in% trainData$`.RNID`]
            trainData[, .RNID := NULL]
            evalData[, .RNID := NULL]
        }else{
            evalData <- NULL
        }

        # put everthing into a list to be returned
        if(DT){
            res <- list(trainData = trainData,
                        testData = testData,
                        evalData = evalData
            )
        }else{
            res <- list(trainData = as.data.frame(trainData),
                        testData = as.data.frame(testData),
                        evalData = as.data.frame(evalData)
            )
        }
    }
    gc()
    return(res)
}

