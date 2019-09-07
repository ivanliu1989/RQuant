#' Find Scripts that contain an object or package-dependency by parsing them
#'
#' Find scripts that have an object or function that you know the name or package of
#'
#' @details Parses scripts and finds which scripts have a sought object, such as the string \code{'password123'}
#' or a functional dependency on a package, e.g. \pkg{RJDBC}. \cr
#' This function actually reads through scripts and will discard scripts that do
#' not run due to typos and bad syntax. \cr
#' This function will NOT find a pattern such as \code{'x <- 3^2'}- for that task,
#' use \code{\link{findScriptsByPattern}}. \cr
#' NOTE: This function cannot distinguish between functions from different
#' packages that have the similar names.
#'
#'@return A data.frame with five columns: \cr
#' script - The name of the script containing the target, \cr
#' linenum - The line number on which the target is found, \cr
#' col - the number of characters into the line that the target can be found, \cr
#' text - the target matched, \cr
#' line - the target within its context
#'
#'@param path a character vector of full path names; the default corresponds to
#' the working directory, \code{\link{getwd}}. Tilde expansion
#' (see \code{\link{path.expand}}) is performed. Missing values will be ignored.
#'@param filename_pattern a regular expression- default is \code{'.R$'}. Only file
#' names which match the regular expression will be returned.
#'@param recursive logical. Should the listing recurse into directories?
#'@param pkg The name of a \emph{currently loaded} package passed as a string.
#'@param code_target character string containing a regular expression to be matched against code
#' while \emph{parsing} through scripts. If a character vector of length 2 or more is supplied,
#'  the first element is used with a warning. If looking for a \emph{pattern}, try \code{\link{findScriptsByPattern}}.
#'@param func_pattern character string containing a regular expression to be matched
#' against the functions found in the package chosen in the argument \code{pkg}.
#' If a character vector of length 2 or more is supplied, the first element is used with a warning.
#'
#'@export
findScriptsByParsing <- function(path = ".",
                                 filename_pattern = ".R$",
                                 recursive = FALSE,
                                 pkg = NULL,
                                 code_target,
                                 func_pattern = ""){

    if(is.null(pkg) & func_pattern != "") stop("func_pattern is chosen but pkg is empty. Choose a package.")

    file_shortlist <- list.files(path = path,
                                 pattern = filename_pattern,
                                 recursive = recursive,
                                 full.names = TRUE)

    if(length(file_shortlist) < 1) stop("No files were found matching that path and filename pattern")

    result <- list()

    if( !missing(code_target)){
        if( is.null(code_target)) stop("code_target is NULL. Pattern was discarded. Did you mean to use list.files()?")
        if( any(is.na(code_target))) stop("code_target is NA. Pattern was discarded. Did you mean to use list.files()?")
        if( any(code_target == "" )) stop("code_target contained empty string. Pattern was discarded. Did you mean to use list.files()?")
    }else{code_target <- NULL}


    for (scriptname in file_shortlist){

        target_funcs <- if(!is.null(pkg)) {
            unlist(lapply(pkg, function(x) ls(paste0("package:",x), pattern = func_pattern)))
        }else{NULL}

        alltargets <- c(target_funcs, pkg, code_target )

        if (length(alltargets) < 1) stop("Choose one or more targets or a package")

        t <- try(getParseData( parse(file = scriptname,keep.source = TRUE)),silent = TRUE)
        if("try-error" %in% class(t)) next()


        tmp <- getParseData( parse(file = scriptname,keep.source = TRUE))

        r1 <- tmp$text %in% alltargets

        instances <- tmp[r1  , c(1,2,9)]
        names(instances) <- c("linenum", "col", "text")

        lns <- readLines(scriptname, warn = FALSE)
        lns <- lns[instances$linenum]

        instances <- data.frame(instances, line = lns)

        scrpt <- gsub("//","/",scriptname)

        file_result <- if(nrow(instances)== 0 ){NULL
        }else{data.frame(script = scrpt, instances)}

        result[[scriptname]] <- file_result
    }

    result <- data.table::rbindlist(result)
    if(!nrow(result) < 1) result[ , script := format(script, justify = "left")]

    if(nrow(result) < 1) message("No instances found!")

    result[]
}

#' Find Scripts that contain a variable or depend on a package by parsing them
#'
#' Find scripts that have a pattern that you specify with a regular expression.
#'
#' @details Scans scripts and finds which scripts have a sought pattern, E.G. \code{'x <- x^2'}. \cr
#' This function scans through scripts and will include obsolate scripts that do
#'  not run due to typos and bad syntax in its search. \cr
#' This function will find a pattern such as \code{'x <- x^2'}, unlike \code{\link{findScriptsByPattern}}.
#'
#'@return A data.frame with four columns: \cr
#' script - The name of the script containing the target, \cr
#' linenum - The line number on which the target is found, \cr
#' col - the number of characters into the line that the target can be found, \cr
#' line - the target within its context
#'
#'@param path a character vector of full path names; the default corresponds to
#' the working directory, \code{\link{getwd}}. Tilde expansion
#' (see \code{\link{path.expand}}) is performed. Missing values will be ignored.
#'@param filename_pattern a regular expression- default is \code{'.R$'}. Only file
#' names which match the regular expression will be returned.
#'@param recursive logical. Should the listing recurse into directories?
#'@param code_pattern character vector of regular expressions (or simp;ly characters) to be matched against text in the script
#' while \emph{scanning} through scripts. If a character vector of length 2 or more is supplied,
#'  the first element is used with a warning.
#'
#'@export
findScriptsByPattern <- function(path = ".",
                                 filename_pattern = ".R$",
                                 recursive = FALSE,
                                 code_pattern){


    file_shortlist <- list.files(path = path,
                                 pattern = filename_pattern,
                                 recursive = recursive,
                                 full.names = TRUE)

    if(length(file_shortlist) < 1) stop("No files were found matching that path and filename pattern")

    result <- list()

    if( missing(code_pattern)) stop("code_pattern is missing. Did you mean to use list.files()?")
    if( is.null(code_pattern)) stop("code_pattern is NULL. Pattern was discarded. Did you mean to use list.files()?")
    if( any(is.na(code_pattern))) stop("code_pattern is NA. Pattern was discarded. Did you mean to use list.files()?")
    if( any(code_pattern == "" )) stop("code_pattern contained empty string. Pattern was discarded. Did you mean to use list.files()?")


    for (scriptname in file_shortlist){

        tmp <- readLines(scriptname, warn = FALSE)

        if (length(code_pattern) > 1){
            pos <- unlist(lapply(tmp, function(x) {

                m <- unlist(lapply(code_pattern, function(y){
                    gregexpr(pattern = y, text = x, fixed = TRUE)[[1]][1]
                }))

                if(all(m < 0)) {m1 <- -1
                }else{m1 <- min(m[m>0])}

            }

            ))
        }else{
            pos <- unlist(lapply(tmp, function(x) {gregexpr(pattern = code_pattern, text = x, fixed = TRUE)[[1]][1]}))

        }

        lns <- tmp[ pos > 0]

        instances <- data.frame(linenum = which(pos > 0), col = pos[pos>0], line = lns )

        scrpt <- gsub("//","/",scriptname)

        file_result <- if(nrow(instances) == 0 ){NULL
        }else{data.frame(script = scrpt, instances)}


        result[[scriptname]] <- file_result
    }

    result <- data.table::rbindlist(result)
    if(!nrow(result) < 1) result[ , script := format(script, justify = "left")]

    if(nrow(result) < 1) message("No instances found!")

    result[]
}
