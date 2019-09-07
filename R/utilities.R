#' Setup the RStuido Environment for New Users
#'
#' Automatically generate \code{.Rprofile} and \code{.Renviron} for basic settings of RStudio development environment. The files will be created under user's root path (e.g. \code{'/home/user1/'})
#' For the first time user, please also see \code{'installCommonPackages()'} function for installing common packages.
#'
#' @examples
#' firstTimeSetup()
#'
#' @seealso \link{installCommonPackages}
#' @export
firstTimeSetup <- function()
{
    rprof = paste0('/home/', Sys.info()[7], '/.Rprofile')
    renvir = paste0('/home/', Sys.info()[7], '/.Renviron')
    shinypath = paste0('/home/', Sys.info()[7], '/ShinyApps')
    if(file.exists(rprof)){
        warning("The existing .Rprofile has been found! Please manually update the file as required.")
        file.edit(rprof)
    }else{
        sink(rprof)
        cat(
            ".First <- function(){
rm(list = ls())
gc()
options(stringsAsFactors = F)
.libPaths(c('/usr/share/R/library'))
# needed to zip up stuff as it is in a non-standard location
Sys.setenv(JAVA_HOME='/usr/lib/jvm/java-7-oracle/') # for 64-bit version
Sys.setenv(R_ZIPCMD='/usr/bin/zip')
Sys.setenv(TZ = 'GMT')
Sys.setenv(ORA_SDTZ = 'GMT')
if('RQuant' %in% utils::installed.packages()[, 'Package']) library(RQuant)
if('RQuantAPI' %in% utils::installed.packages()[, 'Package']) library(RQuantAPI)
if('RQuantTrader' %in% utils::installed.packages()[, 'Package']) library(RQuantTrader)
if('RQuantModelStore' %in% utils::installed.packages()[, 'Package']) library(RQuantModelStore)
cat('\\nWelcome ',Sys.info()[7], ' at', date(), '\\n')
}
")
        sink()
        file.edit(rprof)
        cat("The .Rprofile has been created in root dir, please close it out if no further changes required for the file.")
    }

if(file.exists(renvir)){
    warning("The existing .Rprofile has been found! Please manually update the file as required.")
    file.edit(renvir)
}else{
    sink(renvir)
    cat(
        "R_LIBS_USER=/usr/share/R/library"
    )
    sink()
    file.edit(renvir)
    cat("The .Rprofile has been created in root dir, please close it out if no further changes required for the file.")
}
# Sys.getenv('R_LIBS_USER')

if(dir.exists(shinypath)){
    warning("ShinyApps exists already.")
}else{
    dir.create(shinypath, recursive = TRUE, mode = '0755')
}

if(!dir.exists('~/Common/')){
    system('ln -s /home/Common')
    updateDirFilePermissions('~/Common/')
}
}


#' Install Common Packages
#'
#' Automatically check and install a list of common packages required for financial modeling, data science, statistical analysis and etc. And choose to update existing libraries.
#' Please make sure you've already run \code{'firstTimeSetup()'} before installing packages. The function helps you to re-locate your library path to a shared folder.
#'
#' @param update if the library exists, try to update the library instead. Default \code{FALSE}
#'
#' @examples
#' Sys.getenv('R_LIBS_USER')
#' installCommonPackages(update = FALSE)
#'
#' @seealso \link{firstTimeSetup}
#' @export
installCommonPackages <- function(update = FALSE)
{
    commonPackages <- "webshot,pander,DT,RWeka,igraph,ngram,twitteR,NLP,tm,wordcloud,topicmodels,SnowballC,bursts,qcc,AnomalyDetection,psych,RJDBC,lubridate,plotly,png,sendmailR,ResourceSelection,FNN,caret,digest,DistributionUtils,dplyr,e1071,ecodist,energy,evaluate,expm,fAssets,fBasics,fCopulae,FGN,FinancialInstrument,fMultivar,foreach,forecast,formatR,fPortfolio,fracdiff,FRAPO,fUnitRoots,gdata,GeneralizedHyperbolic,ggplot2,git2r,gplots,gss,gtable,gtools,highr,htmltools,htmlwidgets,httpuv,httr,IBrokers,iterators,jsonlite,kernlab,knitr,ks,akima,assertthat,AUC,BH,bitops,blotter,brew,caTools,cccp,chron,colorspace,crayon,cubature,curl,data.table,DBI,DEoptimR,devtools,dichromat,labeling,lazyeval,ltsa,magrittr,mailR,markdown,memoise,mime,misc3d,mnormt,modopt.matlab,multicool,munsell,mvnormtest,mvtnorm,nloptr,numDeriv,openssl,PearsonDS,PerformanceAnalytics,plyr,PortfolioAnalytics,praise,pROC,quadprog,Quandl,quantmod,quantstrat,R.methodsS3,R.oo,R.utils,R6,RColorBrewer,RcppArmadillo,RCurl,registry,reshape2,rgl,Rglpk,rJava,rneos,robustbase,ROCR,RODBC,ROI,ROI.plugin.glpk,ROI.plugin.quadprog,ROML,ROML.portfolio,roxygen2,Rsolnp,rstudioapi,Rsymphony,rugarch,RUnit,scales,scenportopt,shiny,SkewHyperbolic,slam,sn,sourcetools,sp,spd,stabledist,stringi,stringr,testthat,tibble,tidyjson,timeDate,timeSeries,truncnorm,tseries,TTR,urca,whisker,withr,xgboost,XML,xtable,xts,yaml,zoo,base,boot,class,cluster,codetools,compiler,datasets,foreign,graphics,grDevices,grid,KernSmooth,lattice,MASS,Matrix,methods,mgcv,nlme,nnet,parallel,rpart,spatial,splines,survival,tcltk"
    commonPackages <- strsplit(commonPackages, split = ",")[[1]]
    allPkgs = installed.packages(lib.loc = .libPaths())
    for (i in 1:length(commonPackages)) {
        cat(commonPackages[i], '...\n')
        if (!commonPackages[i] %in% allPkgs[, "Package"]) {
            tryCatch({install.packages(commonPackages[i])})
        }
        else {
            if(update) tryCatch({update.packages(commonPackages[i])})
        }
    }
}



#' Create and print out .gitignore
#'
#' @description
#' A function to print out all suggested file types that need to be added to .gitignore file.
#' Or users can use the function to create or overwrite the .gitignore file directly under current work directory.
#'
#' @param createFile create or overwrite the .gitignore file in current work directory
#' @param openFile open the .gitigore file or not
#' @param additional file types to be ignored by git
#'
#' @examples
#' createGitIgnore(createFile = TRUE, openFile = TRUE, ignoreList = c("*.RData", "*.jpg"))
#'
#' @export
createGitIgnore <- function(createFile = FALSE, openFile = TRUE, ignoreList = NULL){
    ignore.list = unique(c(c(".Rproj.user",".Rhistory",".RData","*.csv","*.rda",
                             "*.xlsx","*.txt","*.png","packrat/lib*/","*.log"),ignoreList))
    ignore.list = paste(ignore.list, collapse = "\n")
    if(createFile){
        if(!file.exists(".gitignore")){
            sink(".gitignore")
            cat(ignore.list)
            sink()
        }
    }
    cat(ignore.list)
    if(openFile & file.exists(".gitignore")) file.edit(".gitignore")
}



#' List functions within a loaded package
#'
#' @return A vector of function names within a loaded package
#'
#' @param pkg The name of a package as a string
#' @param pattern A regex expression to pass to grep to
#' include only certain package
#' @param invert A logical indicating whether to return names that match the
#' provided pattern (TRUE) or that do not match the pattern (FALSE)
#'
#' @examples
#' library(ggplot2)
#'
#' listFns("ggplot2")
#' listFns("ggplot2", pattern = "^theme")
#' listFns("ggplot2", pattern = "^[Stat|theme|geom|Geom|scale]", invert = TRUE)
#'
#' @export
listFns <- function(pkg, pattern = NULL , invert = FALSE){

    if (is.null(pkg)){
        stop("pkg cannot be NULL")
    }

    if(!is.character(pkg) | length(pkg) != 1){
        stop("pkg must be a character string of length one!")
    }

    if (!pkg %in% sub("package:","",grep("package:",search(), value = TRUE))){
        stop("pkg must be a character string of the name of a LOADED package!")
    }

    if( (!is.character(pattern) & !is.null(pattern)) | length(pattern) > 1){
        stop("pattern must be a character string of length one!")
    }

    if(!is.logical(invert)){
        stop("invert must be a logical!")
    }

    eval.command <- paste0("package:",pkg)

    all.functions.in.pkg <- ls(paste0(eval.command))

    if (!is.null(pattern)){
        res <- grep(pattern, all.functions.in.pkg, value = TRUE, invert = invert)
    }else{
        res <- all.functions.in.pkg
    }
    res
}

#' Manipulaton of Directories and File Permissions
#'
#' These functions provide a low-level interface to the computer's file system.
#'
#' @param path a character vector containing a single path name.
#' @param recursive logical. Should elements of the path other than the last be created? If true, like the Unix command mkdir -p.
#' @param mode the mode to be used on Unix-alikes: it will be coerced by as.octmode. For Sys.chmod it is recycled along paths.
#'
#' @examples
#' updateDirFilePermissions(getwd(), recursive = TRUE, mode = "0777")
#'
#' @export
updateDirFilePermissions <- function(path = getwd(), recursive = TRUE, mode = "0777"){
    if(dir.exists(path)){

        files <- list.files(path, all.files = T, full.names = T, recursive  = recursive)
        dirs <- list.dirs(path, full.names = T, recursive  = recursive)

        sapply(files, function(x) Sys.chmod(x, mode = "0777", use_umask = F))
        sapply(dirs, function(x) Sys.chmod(x, mode = "0777", use_umask = F))

        cat(paste0("The permissions under ", path, " have been successfully updated. (", length(files), " files | ", length(dirs), " folders)"))

    }else{
        cat("The dirctory does not exist!")
    }
}


#' Capital initial for each word
#'
#' Capital initial for each word
#'
#' @export
simpleCap <- function(x) {
    x = gsub("_", " ", x)
    x = tolower(x)
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}
