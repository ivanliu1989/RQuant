#' Time steps in a script
#'
#' This function allows you to track the time it takes to run different steps in a script / function / etc.
#'
#' @param entryName The Step to be run
#' @param timingsTable A timingsTable to be added to
#' @param printMsg Whether or not the \code{entryName} should be printed to the screen
#'
#' @return A data frame containing the tasks performed and when they were started. If \code{timingsTable}
#' is provided then this Will be appended to.
#'
#' @examples
#' # Standard Usage
#' timingsTable = addToTimingsTable("Start")
#'
#' # print out the msg
#' timingsTable = addToTimingsTable("Start", NULL, T)
#'
#' # Multiple steps
#' timingsTable = addToTimingsTable("Start")
#' timingsTable = addToTimingsTable("Another Step", timingsTable)
#'
#' @export
addToTimingsTable = function(entryName = "Start", timingsTable = NULL, printMsg = F, printTime = F){

    # TODO: This doesn't work. Replace this with a permanent global solution for setting timezone
    #oldTZ <- Sys.getenv('TZ')
    #Sys.setenv(TZ='Australia/Melbourne')

    newEntry = data.frame(Task = entryName, Time = Sys.time())
    if(is.null(timingsTable)){
        timingsTable = newEntry
    }else{
        timingsTable = rbind(timingsTable, newEntry)
    }
    if(printMsg){

        if(printTime){
            # TODO: Display time elapsed since last call
            #if (dim(timingsTable)[1] > 1)   # More than one entry in timings table
            #{}
            cat(paste0(as.character(newEntry$Time), " - "))
        }

        cat(paste0(entryName,"\n"))
    }

    #Sys.setenv(TZ=oldTZ)
    return(timingsTable)
}


#' Convert a timings table to a final timings table
#'
#' Take in a table created by \code{addToTimingsTable} and add useful information to it like time elapsed
#'
#' @param timingsTable A timingsTable created by \code{addToTimingsTable}
#'
#' @return A data frame containing the tasks performed, when they were
#' started and how long they took to run
#'
#' @examples
#' timingsTable = addToTimingsTable("Start")
#' timingsTable = addToTimingsTable("Step 1", timingsTable)
#' Sys.sleep(4)
#' prepareTimingTable(timingsTable)
#'
#' @export
prepareTimingTable = function(timingsTable){
    steps = nrow(timingsTable)
    runTimeReplaceIds = 1:(steps + 1)
    timingsTable = addToTimingsTable("Finished", timingsTable)

    # empty run time
    timingsTable$RunTime = NA

    # calculate run times
    minsElapsed = difftime(timingsTable$Time[2:(steps+1)], timingsTable$Time[1:(steps)], units = "mins")
    minsElapsed = c(minsElapsed, difftime(max(timingsTable$Time), min(timingsTable$Time),units = "mins"))
    minsElapsed = paste0(round(minsElapsed, 2), "mins")

    # enter run times
    timingsTable$RunTime[runTimeReplaceIds] = minsElapsed

    return(timingsTable)
}


