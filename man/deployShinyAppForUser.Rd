% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shinyUtilities.R
\name{deployShinyAppForUser}
\alias{deployShinyAppForUser}
\title{Deploy a shiny app for a single user}
\usage{
deployShinyAppForUser(appDevPath, appTmpPath, appProdPath = paste0("/home/",
  Sys.info()[7], "/ShinyApps/"), user = Sys.info()[7], appName, versionNo,
  changes, sendEmail = T)
}
\arguments{
\item{appDevPath}{The file path where the current dev version shiny app located}

\item{appTmpPath}{A temporary location that the shiny app will be moved to}

\item{appProdPath}{The production path that the shiny app will be deployed for the user}

\item{user}{User details of the shiny app}

\item{appName}{App's name}

\item{versionNo}{Version No of shiny app}

\item{changes}{Changes that have been updated for the shiny app}

\item{sendEmail}{Send an email to the user after the deployment completion}
}
\description{
Deploy a shiny app for a single user
}

