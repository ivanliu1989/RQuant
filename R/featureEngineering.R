#' Get the season for a particular Date
#'
#' This is used for the extractDateFeatures default function for retrieving a season. This is currently using Australian-based seasons.
#'
#' It will assume the following mapping:
#'  Dec - Feb: Summer
#'  Mar - May: Autumn
#'  Jun - Aug: Winter
#'  Sep - Nov: Spring
#'
#' @param date A date containing a vector of dates to compute the seasons for.
#' @return The season (Summer, Autumn, Winter, Spring) as a character vector.
#' @examples
#' getSeasonFromDate(Sys.Date())
#' @export
getSeasonFromDate <- function(date) {
    m = floor((month(date) %% 12) / 3)
    return (ifelse(is.na(m),"None",ifelse(m==0,"Summer",ifelse(m==1,"Autumn",ifelse(m==2,"Winter","Spring")))))
}

#' Get the Part of Day for a particular Date
#'
#' This is used for the extractDateFeatures default function for retrieving the part of day. This is currently using a user-specified time range. Can be modified and substituted
#' for use in the extractDateFeatures if required.
#'
#' It will assume the following mapping:
#'  5am -< 12pm Morning
#'  12pm -< 5pm Afternoon
#'  5pm -< 9pm Evening
#'  9pm -< 5am Night
#'  @param date A date containing a vector of dates to compute the part of day for.
#'  @return The part of day associated to each date (Morning, Afternoon, Evening or Night).
#'  @examples
#'  getPartOfDayFromDate(Sys.Date())
#'  @export
getPartOfDayFromDate <- function(date) {
    hr = hour(date)
    return (ifelse(is.na(hr),"None",ifelse(hr < 5 | hr >= 21, "Night", ifelse(hr < 12 & hr >= 5, "Morning", ifelse(hr < 17 & hr >= 12, "Afternoon", "Evening")))))
}

#' Get the Financial Week from a particular Date
#' (Not working yet!)
#' This is used for the extractDateFeatures default function for retrieving the financial year. Based on the financial year starting on 1st July of every year.
#'
#' @param date A date containing a vector of dates to compute the financial year for.
#' @return The financial year associated to each date.
#'
#' @examples
#' getFinancialWeekFromDate(as.Date("01/07/2016","%d/%m/%Y"))
#' getFinancialWeekFromDate(as.Date("26/06/2016","%d/%m/%Y"))
#' @export
getFinancialWeekFromDate <- function(date) {
    yr = year(date)
    m = month(date)
    firstWk = lubridate::floor_date(as.Date(paste0("01/07/", ifelse(m<7,yr-1,yr)), format = "%d/%m/%Y"), "week") # start on the Sunday of the 1st July week.
    curWk = lubridate::floor_date(date)
    return (ifelse(is.na(date),NA,floor(as.numeric(curWk - firstWk, units = "weeks"))))
}

#' Get the Financial Year from a particular Date
#'
#' This is used for the extractDateFeatures default function for retrieving the financial year. Based on the financial year starting on 1st July of every year.
#'
#' @param date A date containing a vector of dates to compute the financial year for.
#' @return The financial year associated to each date.
#' @examples
#' getFinancialYearFromDate(as.Date("26/06/2016","%d/%m/%Y"))
#' getFinancialYearFromDate(as.Date("26/07/2016","%d/%m/%Y"))
#' @export
getFinancialYearFromDate <- function(date) {
    yr = year(date)
    m = month(date)
    return (ifelse(is.na(date), NA, ifelse(m >= 7, yr + 1, yr)))
}

#' Extract Date Features from a data.frame or data.table
#'
#' This function is used to construct new date features by decomposing the elements of a date/time.
#' Currently it supports decomposing second, minute, hour, day, week day, week number, month, year, day in year, season, part of day, financial year.
#' It is designed to be flexible via the use of functions being passed into the function. So if there is a requirement to calculate the dates differently, these can be plugged in
#' via user-defined functions without the need to change the function.
#'
#' @param d A data.frame or data.table containing the data set.
#' @param features A character vector containing a list of features to process. If left NULL, will choose ALL the date fields within the data set. Can optionally use regular
#' expression matching to derive the list of features by prepending it with a ~ (refer to Examples).
#' @param decompose A character vector containing the list of features to decompose within the date.
#'
#' @return A data.frame or data.table containing an augmented list of processed decomposed date features.
#' @examples
#' sample.df <- data.frame(ID = runif(100, 0, 10000), EFF_DATE = Sys.time() + runif(100, 0, 24*60*60*100), EFF_TO = Sys.time() + runif(100, 24*60*60*100+1, 24*60*60*1000))
#' sample.extract <- extractDateFeatures(sample.df) # All date features get decomposed with all the attributes
#' sample.extract <- extractDateFeatures(sample.df, features = "~*DATE", decompose = c("season","partofday","fyear"))
#' @export
extractDateFeatures <- function(d, features = NULL, exclude = NULL, remove.original = FALSE, decompose = c("second", "minute", "hour", "day", "wday", "week", "month", "year", "yday", "season", "partofday", "fyear"),
                                season = getSeasonFromDate, partofday = getPartOfDayFromDate, fyear = getFinancialYearFromDate) {
    require(lubridate)
    require(data.table)

    data.class <- class(d)

    if (!("data.frame" %in% data.class)) {
        stop("Cannot process non-data frame/data table datasets.")
    }

    bDF <- FALSE
    if (data.class[1] == "data.frame") {
        bDF <- TRUE
    }

    data <- data.table(d)
    all.features <- colnames(data)

    if (is.null(features)) {
        # transform all date features if left NULL
        date.features <- colnames(data)[which(sapply(data, function(x) class(x)[1]) %in% c("POSIXct"))]
    } else {
        # transform the features if regexp is applied
        date.features <- vector()
        for (f in features) {
            if (grepl("^~.*", f)) {
                f.features <- all.features[grep(substring(f, 2), all.features)]
            } else {
                f.features <- all.features[grep(f, all.features, fixed = TRUE)]
            }
            date.features <- c(date.features, f.features)
        }
    }

    if (!is.null(exclude)) {
        # remove these from potential features
        date.features <- date.features[!(date.features %in% exclude)]
    }

    # Generate some new date features
    pb <- txtProgressBar(min = 0, max = length(date.features)+1, initial = 0, char = "=", style = 3)
    iter <- -1

    if (length(date.features) > 0) {
        for (feat in decompose) {
            iter <- iter + 1
            setTxtProgressBar(pb, value = iter)
            # check if a direct application of lubridate + customised functions
            if (feat %in% c("second", "minute", "hour", "day", "wday", "week", "month", "year", "yday", "season", "partofday", "fyear")) {
                data[, paste0(date.features,"_",feat) := lapply(.SD, eval(parse(text = feat))), .SDcols = date.features]
            }
        }
    }

    setTxtProgressBar(pb, value = length(date.features)+1)

    if (remove.original) {
        rsapply(data, date.features, rsRemove)
    }

    if (bDF) {
        setDF(data)
    }

    return (data)
}

#' Generate One Hot Encoding
#'
#' This function applies one hot encoding to factors and/or character based columns in a data frame or data table.
#'
#' In addition, it has the ability to impute character columns that are missing or will be empty after conversion.
#'
#' @param d A data frame or data table containing the data set.
#' @param features A character vector containing a list of features to process. If left NULL, will choose ALL the factor or character fields within the data set. Can optionally use regular
#' expression matching to derive the list of features by prepending it with a ~ (refer to Examples).
#' @param max.levels An integer containing the maximum number of unique levels to consider to generate a OHE for. For example, it might not make sense to OHE a column containing 10,000 unique values for sparsity reasons.
#' @param convert.na A character string to impute missing character values to. Default: "NA"
#' @param convert.empty A character string to impute empty character values to (trim operation is applied beforehand). Default: "Empty"
#' @param remove.original A logical dictating whether or not we want to remove the original column that we have applied OHE on. Default: FALSE
#' @param full.rank A logical dictating whether or not we want to make the OHE features linearly dependent or independent. The value will depend on the type of model being built. For tree-based methods it can be valuable
#' to keep it FALSE, for linear models it may be better to set it to TRUE. Default: FALSE
#' @return A data frame or data table containing the transformed data set with the OHE features augmented.
#' @examples
#'
#' sample.df <- data.frame(ID = floor(runif(100, 0, 10000)),
#' EFF_DATE = Sys.time() + runif(100, 0, 24*60*60*100),
#' EFF_TO = Sys.time() + runif(100, 24*60*60*100+1, 24*60*60*1000),
#' CUST_SEGMENT_CHR = as.character(floor(runif(100,0,10))),
#' STATE_NAME = ifelse(runif(100,0,1) < 0.56, 'VIC', ifelse(runif(100,0,1) < 0.44,'NSW', 'QLD')),
#' REVENUE = floor(rnorm(100, 500, 200)),
#' NUM_FEAT_1 = rnorm(100, 1000, 250),
#' NUM_FEAT_2 = rnorm(100, 20, 2),
#' NUM_FEAT_3 = floor(rnorm(100, 3, 0.5)),
#' NUM_FEAT_4 = floor(rnorm(100, 100, 10)),
#' RFM_SEGMENT = factor(x = letters[floor(runif(100,1,6))], levels = c("a","b","c","d","e")))
#'
#' generateOHE(sample.df) # generate OHE on all characters or factors
#' generateOHE(sample.df, features = c("~CUST*", "~*SEGMENT*")) # generate only on those matching the regex
#' generateOHE(sample.df, features = c("~CUST*", "~*SEGMENT*"), convert.na = "Missing") # convert missing values to 'Missing' before OHE
#' generateOHE(sample.df, features = "~*SEGMENT*", convert.na = "Missing", full.rank = TRUE) # satisfy full rank requirements
#' @export
generateOHE <- function(d, features = NULL, exclude = NULL, max.levels = 100, convert.na = "NA", convert.empty = "Empty", remove.original = FALSE, full.rank = FALSE) {
    require(utils)

    data.class <- class(d)

    if (!("data.frame" %in% data.class)) {
        stop("Cannot process non-data frame/data table datasets.")
    }

    # make sure convert.na and convert.empty are characters
    convert.na <- as.character(convert.na)
    convert.empty <- as.character(convert.empty)

    bDF <- FALSE
    if (data.class[1] == "data.frame") {
        bDF <- TRUE
    }

    data <- data.table(d)
    all.features <- colnames(data)

    if (is.null(features)) {
        # transform all factor or character features if left NULL
        ohe.features <- colnames(data)[which(sapply(data, function(x) class(x)[1]) %in% c("factor","character"))]
    } else {
        # transform the features if regexp is applied
        ohe.features <- vector()
        for (f in features) {
            if (grepl("^~.*", f)) {
                f.features <- all.features[grep(substring(f, 2), all.features)]
            } else {
                f.features <- all.features[grep(f, all.features, fixed = TRUE)]
            }
            ohe.features <- c(ohe.features, f.features)
        }
    }

    if (!is.null(exclude)) {
        # remove these from potential features
        ohe.features <- ohe.features[!(ohe.features %in% exclude)]
    }

    pb <- txtProgressBar(min = 0, max = length(ohe.features)+1, initial = 0, char = "=", style = 3)

    # Process the one hot encoding
    iter <- -1
    for (col in ohe.features) {
        iter <- iter + 1
        setTxtProgressBar(pb, value = iter)
        if (!class(data[[col]]) %in% c("factor","character")) {
            warning(paste0(col, " is not a factor or character. Skipping this feature."))
            next
        }

        # Convert the NA's to something more meaningful (this will be converted into a separate factor level)
        if (!is.null(convert.na) & sum(is.na(data[[col]])) > 0) {
            set(data, which(is.na(data[[col]])), col, convert.na)
        }

        # Convert the blanks to something more meaningful (this will be converted into a separate factor level)
        # We only convert character types
        if (!is.null(convert.empty) & class(data[[col]]) == "character" & sum(sapply(trimws(data[[col]]), nchar) == 0) > 0) {
            set(data, which(sapply(trimws(data[[col]]), nchar) == 0), col, convert.empty)
        }

        # Create the dummy variables if required
        dummy <- as.data.frame(data[, lapply(.SD, unique), .SDcols = col])[,1]

        if (class(dummy)[1] == "factor") {
            # change it to chracter for processing
            dummy <- as.character(dummy)
        }

        # Filter out the NA column if any
        dummy <- dummy[!is.na(dummy)]

        if (length(dummy) > max.levels) {
            warning(paste0(col, " contains more than ", max.levels, " factor levels. Skipping the one-hot encoding of this feature."))
            next
        } else if (length(dummy) <= 1) {
            warning(paste0(col, " contains exactly 1 factor level. Skipping the one-hot encoding of this feature."))
            next
        }

        if (full.rank) {
            dummy <- dummy[-length(dummy)]
        }

        print(paste0(trimws(gsub("[^[:alnum:]]", ".", gsub("[[:space:]]",".",col))), '_', trimws(gsub("[^[:alnum:]]", ".", gsub("[[:space:]]",".", dummy)))))
        data[, paste0(trimws(gsub("[^[:alnum:]]", ".", gsub("[[:space:]]",".",col))), '_', trimws(gsub("[^[:alnum:]]", ".", gsub("[[:space:]]",".", dummy)))) :=
                 lapply(dummy, function(x) ifelse(data[, col, with = F] == x, 1, 0))]

        # remove the original factor
        if (remove.original) {
            suppressWarnings(data[, col := NULL, with = F])
        }
    }

    setTxtProgressBar(pb, value = length(ohe.features)+1)

    if (bDF) {
        setDF(data)
    }

    return (data)
}

#' Convert Factor features to Numeric features
#'
#' A function to convert the factor levels to a numeric value. It will take the ordering of the factor into account, so if the factor is ordinal then
#' the resulting numeric value will also capture this relationship. If suffix is set to an empty string or NULL, then an in-place operation occurs such
#' that the original feature will be overwritten. Otherwise the default behaviour is to have the numeric feature in a different column (appended with _enc).
#'
#' @param d A data frame or data table containing the data set.
#' @param features A character vector containing a list of features to process. If left NULL, will choose ALL the factor fields within the data set. Can optionally use regular
#' expression matching to derive the list of features by prepending it with a ~ (refer to Examples). Default: NULL.
#' @param suffix A character string containing what to append to each converted feature. If set to NULL or an empty string then an in-place operation occurs. Default: _enc.
#' @return A data frame or data table containing the transformed data set with the factor features converted to numeric features.
#' @examples
#' sample.df <- data.frame(ID = floor(runif(100, 0, 10000)),
#' EFF_DATE = Sys.time() + runif(100, 0, 24*60*60*100),
#' EFF_TO = Sys.time() + runif(100, 24*60*60*100+1, 24*60*60*1000),
#' CUST_SEGMENT_CHR = as.character(floor(runif(100,0,10))),
#' STATE_NAME = ifelse(runif(100,0,1) < 0.56, 'VIC', ifelse(runif(100,0,1) < 0.44,'NSW', 'QLD')),
#' REVENUE = floor(rnorm(100, 500, 200)),
#' NUM_FEAT_1 = rnorm(100, 1000, 250),
#' NUM_FEAT_2 = rnorm(100, 20, 2),
#' NUM_FEAT_3 = floor(rnorm(100, 3, 0.5)),
#' NUM_FEAT_4 = floor(rnorm(100, 100, 10)),
#' RFM_SEGMENT = factor(x = letters[floor(runif(100,1,6))], levels = c("a","b","c","d","e")))
#'
#' generateFactorEncoding(sample.df) # push all the converted factors as _enc
#' generateFactorEncoding(sample.df, suffix = NULL) # in-place conversion
#' generateFactorEncoding(sample.df, features = "~*") # do all features regardless of type (may not make sense!)
#' @export
generateFactorEncoding <- function(d, features = NULL, exclude = NULL, suffix = "_enc", verbose = FALSE) {
    data.class <- class(d)
    if (is.null(suffix)) {
        suffix = ""
    }

    if (!("data.frame" %in% data.class)) {
        stop("Cannot process non-data frame/data table datasets.")
    }

    bDF <- FALSE
    if (data.class[1] == "data.frame") {
        bDF <- TRUE
    }

    data <- data.table(d)
    all.features <- colnames(data)

    if (is.null(features)) {
        # transform all factor features to numeric if left NULL
        conv.features <- colnames(data)[which(sapply(data, function(x) class(x)[1]) %in% c("factor"))]
    } else {
        # transform the features if regexp is applied
        conv.features <- vector()
        for (f in features) {
            if (grepl("^~.*", f)) {
                f.features <- all.features[grep(substring(f, 2), all.features)]
            } else {
                f.features <- all.features[grep(f, all.features, fixed = TRUE)]
            }
            conv.features <- c(conv.features, f.features)
        }
    }

    if (!is.null(exclude)) {
        # remove these from potential features
        conv.features <- conv.features[!(conv.features %in% exclude)]
    }

    pb <- txtProgressBar(min = 0, max = length(conv.features)+1, initial = 0, char = "=", style = 3)
    iter <- -1
    # Check if we need to convert from categorical to numerical variables
    # Note that this could potentially make disparate sets of factor levels incomparable
    for (col in conv.features) {
        iter <- iter + 1
        setTxtProgressBar(pb, value = iter)
        col.class <- class(data[[col]])[1]
        if (col.class != "factor") {
            convert <- as.numeric(factor(data[[col]]))
        } else {
            convert <- as.numeric(data[[col]])
        }
        data[, (paste0(col, suffix)) := convert]
    }

    setTxtProgressBar(pb, value = length(conv.features)+1)

    if (bDF) {
        setDF(data)
    }

    return (data)
}

#' Clean Numeric Features
#'
#' This function performs some data cleansing on numeric fields. It currently supports centering, scaling, imputation (via mean, median or value).
#'
#' @param d A data frame or data table containing the data set.
#' @param features A character vector containing a list of features to process. If left NULL, will choose ALL the numeric fields within the data set. Can optionally use regular
#' expression matching to derive the list of features by prepending it with a ~ (refer to Examples). Default: NULL.
#' @param num.transform A character vector containing the type of numeric transformation to perform. Can be a combination of SCALE, CENTER or IMPUTE.
#' @param num.impute.method A character string containing the imputation method to select. Can be VALUE, MEAN or MEDIAN.
#' @param num.impute A numeric containing the imputation value to impute (used for value-based imputation).
#' @return A data frame or data table containing the transformed data set with the transformed numeric values.
#' @examples
#' Features use ~ to use regexp, else leave NULL if all numeric else specify exactly the attribute
#' sample.df <- data.frame(ID = floor(runif(100, 0, 10000))),
#' EFF_DATE = Sys.time() + runif(100, 0, 24*60*60*100),
#' EFF_TO = Sys.time() + runif(100, 24*60*60*100+1, 24*60*60*1000),
#' CUST_SEGMENT_CHR = as.character(floor(runif(100,0,10))),
#' STATE_NAME = ifelse(runif(100,0,1) < 0.56, 'VIC', ifelse(runif(100,0,1) < 0.44,'NSW', 'QLD')),
#' REVENUE = floor(rnorm(100, 500, 200)),
#' NUM_FEAT_1 = rnorm(100, 1000, 250),
#' NUM_FEAT_2 = rnorm(100, 20, 2),
#' NUM_FEAT_3 = floor(rnorm(100, 3, 0.5)),
#' NUM_FEAT_4 = floor(rnorm(100, 100, 10)),
#' RFM_SEGMENT = factor(x = letters[floor(runif(100,1,6))], levels = c("a","b","c","d","e")))
#'
#' # Introduce some missing values
#' sample.df$NUM_FEAT_1 <- ifelse(sample.df$NUM_FEAT_1 > 1150, NA, sample.df$NUM_FEAT_1)
#' sample.df$NUM_FEAT_2 <- ifelse(sample.df$NUM_FEAT_2 < 17, NA, sample.df$NUM_FEAT_2)
#' sample.df$NUM_FEAT_3 <- ifelse(sample.df$NUM_FEAT_3 > 3, NA, sample.df$NUM_FEAT_3)
#' sample.df$NUM_FEAT_4 <- ifelse(sample.df$NUM_FEAT_4 > 110 | sample.df$NUM_FEAT_4 < 88, NA, sample.df$NUM_FEAT_4)
#' sample.df$CUST_SEGMENT_CHR <- ifelse(sample.df$CUST_SEGMENT_CHR == '8', NA, sample.df$CUST_SEGMENT_CHR)
#'
#' # Impute all missing numeric features to -1
#' (cleanNumericFeatures(sample.df, num.transform = "IMPUTE", num.impute = -1))
#' # Only choose the CUST_SEGMENT_CHR - will convert it to a numeric
#' (cleanNumericFeatures(sample.df, features = "CUST_SEGMENT_CHR", num.transform = "NONE"))
#' (str(cleanNumericFeatures(sample.df, features = "CUST_SEGMENT_CHR", num.transform = "NONE")))
#' # Supports regular expressions to choose columns, so doing the exact same thing but using a regex:
#' (cleanNumericFeatures(sample.df, features = "~*CHR", num.transform = "NONE"))
#' # Supports mean-valued imputation. If features not supplied, takes all numeric features into consideration.
#' (cleanNumericFeatures(sample.df, num.transform = "IMPUTE", num.impute.method = "MEAN"))
#' # Can also do SCALE/CENTER:
#' (cleanNumericFeatures(sample.df, features = "~*", num.transform = c("SCALE","CENTRE")))
#' # Can also do SCALE/CENTER + combine with imputing values. Note that imputation is ALWAYS done first before SCALE/CENTRE.
#' (cleanNumericFeatures(sample.df, features = "~*", num.transform = c("SCALE","CENTRE", "IMPUTE"), num.impute.method = "MEDIAN"))
#' @export
cleanNumericFeatures <- function (d, features = NULL, exclude = NULL, num.transform = c("SCALE","CENTER","IMPUTE"), num.impute.method = "VALUE", num.impute = -1, verbose = FALSE) {
    # standardise variables for easier comparison
    num.impute.method = toupper(num.impute.method)
    num.transform = toupper(num.transform)
    data.class <- class(d)

    bDF <- FALSE

    if (!("data.frame" %in% data.class)) {
        stop("Cannot process non-data frame/data table datasets.")
    }

    if (data.class[1] == "data.frame") {
        bDF <- TRUE
    }

    # Create a copy as we don't want to modify by reference
    data <- data.table(d)
    all.features <- colnames(data)

    if (is.null(features)) {
        # transform all numeric values if left NULL
        num.features <- colnames(data)[which(sapply(data, function(x) class(x)[1]) %in% c("numeric", "integer"))]
    } else {
        # transform the features if regexp is applied
        num.features <- vector()
        for (f in features) {
            if (grepl("^~.*", f)) {
                f.features <- all.features[grep(substring(f, 2), all.features)]
            } else {
                f.features <- all.features[grep(f, all.features, fixed = TRUE)]
            }
            num.features <- c(num.features, f.features)
        }
    }

    if (!is.null(exclude)) {
        # remove these from potential features
        num.features <- num.features[!(num.features %in% exclude)]
    }

    pb <- txtProgressBar(min = 0, max = length(num.features)+1, initial = 0, char = "=", style = 3)
    iter <- -1
    num.processed <- 0
    num.skipped <- 0
    num.cast <- 0
    for (col in num.features) {
        iter <- iter + 1

        # test whether or not we can cast it to numeric (if character)
        col.class <- class(data[[col]])[1]
        bTest <- suppressWarnings(sum(!is.na(as.numeric(data[[col]]))) == sum(!is.na(data[[col]])))
        if (!bTest || (col.class %in% c("POSIXct", "POSIXt"))) {
            # failed
            warning(paste0("Could not properly cast ", col, " to numeric. Skipping..."))
            num.skipped <- num.skipped + 1
            next
        }

        if (!(col.class %in% c("integer","numeric"))) {
            # convert it to integer/numeric (should pass if it passed the above test)
            data[, col := lapply(.SD, function(x) as.numeric(x)), with = F, .SDcols = col]
            num.cast <- num.cast + 1
        }

        setTxtProgressBar(pb, value = iter)
        # Determine if we need to scale +/- center the variables
        data[, col := scale(data[[col]], scale = ("SCALE" %in% num.transform), center = ("CENTER" %in% num.transform | "CENTRE" %in% num.transform)), with = F]

        if ("IMPUTE" %in% num.transform) {
            # Need to do some imputation
            if (is.null(num.impute.method) | !(num.impute.method %in% c("VALUE","MEAN","MEDIAN"))) {
                # Default method is VALUE based imputation
                # Set to this method if the supplied method is missing and/or invalid
                num.impute.method = "VALUE"
            }
            if (num.impute.method == "VALUE") {
                set(data, which(is.na(data[[col]])), col, num.impute)
            } else if (num.impute.method == "MEAN") {
                impute.mean <- mean(data[[col]], na.rm=TRUE)
                set(data, which(is.na(data[[col]])), col, impute.mean)
            } else if (num.impute.method == "MEDIAN") {
                impute.median <- median(data[[col]], na.rm=TRUE)
                set(data, which(is.na(data[[col]])), col, impute.median)
            }
        }
        num.processed <- num.processed + 1
    }

    setTxtProgressBar(pb, value = length(num.features)+1)

    if (verbose) {
        message(paste0("Finished processing with ", num.processed, " numeric features processed (with ", num.cast, " casted from character types). ", num.skipped, " features skipped due to conversion failures."))
    }

    if (bDF) {
        setDF(data)
    }
    return (data)
}


#' Helper function for rsapply to do Value-based Imputation.
#'
#' Not meant to be used as-is.
#'
#' @export
rsImputeValue <- function(x, v = -1) {
    ifelse(is.na(x), v, x)
}

#' Helper function for rsapply to do Mean Imputation.
#'
#' Not meant to be used as-is.
#'
#' @export
rsImputeMean <- function(x) {
    ifelse(is.na(x), mean(x, na.rm=TRUE), x)
}

#' Helper function for rsapply to do Median Imputation.
#'
#' Not meant to be used as-is.
#'
#' @export
rsImputeMedian <- function(x) {
    ifelse(is.na(x), median(x, na.rm=TRUE), x)
}

#' Helper function for rsapply to do Factor Imputation.
#'
#' Not meant to be used as-is.
#'
#' @export
rsImputeFactor <- function(x) {
    # best used for categorical values
    common <- names(sort(table(x))[1])
    ifelse(is.na(x), common, x)
}

#' Helper function for rsapply to do remove an element.
#'
#' Not meant to be used as-is.
#'
#' @export
rsRemove <- function(x) {
    NULL
}

#' Helper function for rsapply to do print an element.
#'
#' Not meant to be used as-is.
#'
#' @export
rsPrint <- function(x) {
    x
}

#' Helper function for rsapply to do retrieve column names.
#'
#' Not meant to be used as-is.
#'
#' @export
rsGetColumnNames <- function(x) {
    colnames(x)
}

#' Helper function for rsapply to do determine features in a class.
#'
#' Not meant to be used as-is.
#'
#' @export
rsClass <- function(dt, c) {
    colnames(dt)[dt[, lapply(.SD, function(x) class(x)) %in% c]]
}

#' Regular Expression Apply Function for data.table
#'
#' This function allows you to match columns to input into an lapply function based on a regular expression.
#'
#' @usage rsapply(X, M, FUN, ..., assign = NULL, by)
#' @param X a data.table or data.frame object.
#' @param M a character vector containing regular expressions prepended with tilde (~) and/or a fixed string (without the tilde).
#' @param FUN the function to be applied to each element of X. This can be a value if assign is supplied.
#' @param ... optional arguments to FUN.
#' @param assign a character string containing what column name to prepend each assignment to. Can be left an empty string "" for in-place transformation.
#' @param by a character vector of column names to group the operation by.
#' @examples
#'
#' data(iris)
#' iris.dt <- data.table(iris)
#' rsapply(iris.dt, "~*Sepal", as.character, assign = "ch") # build new features with a character conversion, each column prepended with 'ch'
#' rsapply(iris.dt, "~*ch$", 1, assign = "") # different type: will have data.table warning
#' rsapply(iris.dt, "~*ch$", "2", assign = "") # same type: no data.table warning
#' rsapply(iris.dt, "~*ch$", NULL, assign = "") # remove all the columns that end in 'ch'
#' str(rsapply(iris.dt, "~*Sepal", as.character))
#' rsapply(iris.dt, c("~Sepal","~Petal"), quantile, probs = 1:3/4) # calculate the first 3 quantiles for all columns that have Sepal or Petal
#' rsapply(iris.dt, c("~Sepal","~Petal"), quantile, probs = 1:3/4, by = "Species") # calculate the first 3 quantiles for all Sepal or Petal grouped by Species
#' # Find the mean difference between 1st and 3rd quantile of all species for all Length only columns
#' rsapply(
#'   rsapply(
#'     rsapply(iris.dt, c("~Sepal","~Petal"), quantile, probs = c(1,3)/4, by = "Species"),
#'     c("~Sepal","~Petal"), function(x) max(x) - min(x), by = "Species"),
#'   c("~Length"), mean
#' )
#' rsapply(iris.dt, c("~Sepal","~Petal"), mean, by = "Species")[, .(ratio = Sepal.Length / Sepal.Width)] # Chain a new column called ratio which computes the ratio of Sepal Length and Width
#' melt(rsapply(iris.dt, c("~Sepal","~Petal"), mean, by = "Species"), id.vars = "Species") # Naturally can use melt and dcast for pivoting
#' rsapply(rsapply(dt, "~*SEGMENT*", function(x) ifelse(is.na(x), -1, x), assign = "_NEW"), "~*SEGMENT", print) # imputation
#' rsapply(rsapply(dt, "~*SEGMENT*", function(x) ifelse(is.na(x), -1, x), assign = ""), "~*SEGMENT", print) # in place imputation
#'
#' num.col <- colnames(dd)[dd[, lapply(.SD, function(x) class(x)[1]) == "numeric"]] # only get the numeric attributes
#' rsapply(dd, num.col, print) # Print the columns
#' rsapply(dd, num.col, function(x) ifelse(is.na(x),-1,x), assign = "") # in place imputation for numeric only attributes
#' rsapply(dd, rsClass(dd, "numeric"), rsPrint) # fetch only the numeric attributes utilising rsClass
#' @export
rsapply <- function(X, M, FUN, ..., assign = NULL, by, in.place = TRUE) {
    require(data.table)
    # force it to match a function if it's not an assignment
    if (is.null(assign) || class(FUN) == "function") {
        FUN <- match.fun(FUN)
    }
    # do not change by reference if it's not an assignment
    if (is.null(assign) || !in.place) { X <- data.table(X) } else { setDT(X) }
    cols = colnames(X)
    matched <- unlist(lapply(M, function(x) { if (grepl("^~.*", x)) { cols[grep(substring(x,2), cols)]} else { cols[grep(x, cols, fixed = T)]} }))
    if (length(matched) == 0) {
        warning("Failed to match to the supplied matching pattern.")
        return (invisible(0))
    }
    if (is.null(assign)) {
        return (X[, lapply(.SD, FUN, ...), by = by, .SDcols = matched])
    }
    # assign by value (useful for removing elements)
    if (class(FUN) != "function") {
        # does not use SD/by for this kind of value-based assignment
        return (X[, paste0(matched, assign) := FUN])
    }
    return (X[, paste0(matched, assign) := lapply(.SD, FUN, ...), by = by, .SDcols = matched])
}
