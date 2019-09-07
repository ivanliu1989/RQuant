#' Create HTML release notes for email
#'
#' @export
releaseNotes = function(changes){
    if(length(unlist(changes)>0)){
        return(paste0(RQuant::toHTML(paste0("The changes in this update include:")),asHTMLList(changes), collapse = ""))
    }else{
        return("")
    }
}


#' Deploy a shiny app for a single user
#'
#' @param appDevPath The file path where the current dev version shiny app located
#' @param appTmpPath A temporary location that the shiny app will be moved to
#' @param appProdPath The production path that the shiny app will be deployed for the user
#' @param user User details of the shiny app
#' @param appName App's name
#' @param versionNo Version No of shiny app
#' @param changes Changes that have been updated for the shiny app
#' @param sendEmail Send an email to the user after the deployment completion
#'
#' @export
deployShinyAppForUser = function(
    appDevPath,
    appTmpPath,
    appProdPath = paste0("/home/", Sys.info()[7], "/ShinyApps/"),
    user = Sys.info()[7],
    appName,
    versionNo,
    changes,
    sendEmail = T
){
    if(!file.exists(appProdPath)){
        stop("Production path does not exist")
    }
    if(!file.exists(appTmpPath)){
        copyFolderToShinytmp = paste0("cp -avr ",appDevPath, " ", appTmpPath, "")
        system(copyFolderToShinytmp)
        gc()
    }

    usersPath = paste0(appProdPath, "/", user$user, "_", user$randId)
    copyFolderToShinyUser = paste0("cp -avr ",appTmpPath, " ", usersPath)
    userResultsPath = paste0(usersPath, "/results")

    # remove old version
    if(file.exists(usersPath)){
        system(paste0("rm -r ", usersPath))
    }

    system(copyFolderToShinyUser)
    # create a results folder to save csvs
    dir.create(userResultsPath)
    # set write permissions to the folder
    system(paste0("chmod  777  ",usersPath))
    system(paste0("chmod  777  ",userResultsPath))
    write(user$user, paste0(usersPath, "/userConfig"))

    if(sendEmail){
        userDeploymentEmail(user, changes, appName, versionNo, appProdPath)
    }

}

#' Send a standard release email to a user of a shiny app
#'
#' @param user User details of the shiny app
#' @param changes Changes that have been updated for the shiny app
#' @param appName App's name
#' @param versionNo Version No of shiny app
#' @param appProdPath The production path that the shiny app will be deployed for the user
#'
#' @export
userDeploymentEmail = function(
    user,
    changes,
    appName,
    versionNo,
    appProdPath
){
    userId =user$randId
    userName =user$user
    appUrlPath = paste0("",userName,"/",appName)
    subject = paste0(appName," UPDATED version")
    msg = buildhtmlmsg(list(
        toHTML(paste0("Updated version of the ",appName), "h4"),
        toHTML(paste0("The ",appName," has been updated with the following changes:"), "p"),
        releaseNotes(changes),
        toHTML(paste0("What you need to do:"), "h4"),
        toHTML(paste0("You will need to replace your existing link to ",appName," with the new version."), "p"),
        toHTML(paste0("Please save the following into your bookmarks.  This link contains your unique ID so will work for you only:"), "p"),
        toHTML(paste0("<a href='",appUrlPath,"'>",appName," v",versionNo, "</a>")),
        toHTML("Activating your access","h4"),
        toHTML(paste0("Refresh your browser to load the new version of ",appName,".")),
        toHTML("Further Information","h4"),
        toHTML("If you have any questions then don't hesitate to contact ivan.liuyanfeng@gmail.com.")
    ))
    to = unique(c(user$email, "ivan.liuyanfeng@gmail.com"))[1]
    rquantSendEmail(to, subject, msg)
}

#' Deploy a shiny app for all users
#'
#' @param appDevPath The file path where the current dev version shiny app located
#' @param appTmpPath A temporary location that the shiny app will be moved to
#' @param appProdPath The production path that the shiny app will be deployed for the user
#' @param users A list of users of the shiny app
#' @param appName App's name
#' @param versionNo Version No of shiny app
#' @param changes Changes that have been updated for the shiny app
#' @param sendEmail Send an email to each user after the deployment completion
#'
#' @export
deployShinyApp = function(
    appDevPath,
    appTmpPath,
    appProdPath,
    users,
    appName,
    versionNo,
    changes,
    sendEmail = F
){

    # clean up old tmp path
    if(file.exists(appTmpPath)){
        system(paste0("rm -r ", appTmpPath))
    }
    # copy the app to a temp folder
    copyFolderToShinytmp = paste0("cp -avr ",appDevPath, " ", appTmpPath, "")
    system(copyFolderToShinytmp)
    gc()

    # cleanup unwanted files and folders
    if(length(itemsToRemove)>0){
        for(i in 1:length(itemsToRemove)){
            if(file.exists(paste0(appTmpPath, itemsToRemove[i]))){
                system(paste0("rm -r ", appTmpPath, itemsToRemove[i]))
            }
        }
    }

    if(file.exists(appProdPath)){
        system(paste0("rm -r ", appProdPath))
    }
    dir.create(appProdPath)

    indexHtml = '<html><body>
  <p>Nothing to see here.</p>
  </body></html>
  '
    write(indexHtml, file = paste0(appProdPath, "/index.html"))

    gc();cat("\014")
    for(i in 1:nrow(users)){
        deployShinyAppForUser(
            appDevPath,
            appTmpPath,
            appProdPath,
            users[i,],
            appName,
            versionNo,
            changes,
            sendEmail
        )
    }
}

