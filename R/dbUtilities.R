#' Parameterised SQL.
#'
#' @description
#' The core parameter replacement function accepting a list
#' of parameter names to replace and replacing all the
#' occurances in x that have @......
#'
#' @param x A string that represents a query with @paramName to be replaced.
#' @param dataConfig The configuration list, where the names represent the
#' parameter and the values are what will be replaced.
#'
#' @return A string that represents an SQL query.
#'
#' @note Dates have to use the format that is expected by SQL.
#'
#' @seealso \code{\link{format}}, \code{\link{dbGetQuery}}
#'
#' @examples
#' updateSQLwithParams("select * from @table", list(table = "blah"))
#'
#' query.txt<-"
#' select
#'    sum(total_sales_amount) as spend
#'    ,transaction_date
#' from
#'    cedwdm.transaction_product_vw
#' where
#'    transaction_date > '@start.date'
#' group by
#'    transaction_date
#' "
#' my.query<-updateSQLwithParams(
#'    query.txt
#'    , list(start.date = format(Sys.Date() - 7, "%d-%b-%Y" ))
#'    )
#' print(my.query)
#'
#' @export
updateSQLwithParams = function(x, dataConfig){
    if(length(x)>1){
        x = paste0(x, collapse = "\n")
    }
    configNames = names(dataConfig)
    for(n in configNames){
        if(length(dataConfig[[n]])==1){
            x = gsub(pattern = paste0("@", n),
                     replacement = as.character(format(dataConfig[[n]], scientific = F)), x)
        }
    }
    return(x)
}


#' Read in a sql file and update the parameters
#'
#' Given a file path to a sql query, read in the query and update based
#' on a set of parmeters
#'
#' @param path A file path that refers to a sql file
#' @param dataConfig The configuration list, where the names represent the
#' parameter and the values are what will be replaced. will be passed to \code{updateSQLwithParams}
#'
#' @export
readSQLFileAndUpdateParameters = function(path, dataConfig = list()){
    fl = file(path)
    query = updateSQLwithParams(readLines(fl),dataConfig)
    close(fl)
    return(query)
}


#' Get a JSON file
#'
#' @param path A path to pass to \code{\link{file}} which is then passed to
#'  \code{\link{readlines}}
#' @export
getJSONFile = function(path){
    fl = file(path)
    res = fromJSON(readLines(fl))
    close(fl)
    return(res)
}
