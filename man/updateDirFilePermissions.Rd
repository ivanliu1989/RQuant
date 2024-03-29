% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{updateDirFilePermissions}
\alias{updateDirFilePermissions}
\title{Manipulaton of Directories and File Permissions}
\usage{
updateDirFilePermissions(path = getwd(), recursive = TRUE, mode = "0777")
}
\arguments{
\item{path}{a character vector containing a single path name.}

\item{recursive}{logical. Should elements of the path other than the last be created? If true, like the Unix command mkdir -p.}

\item{mode}{the mode to be used on Unix-alikes: it will be coerced by as.octmode. For Sys.chmod it is recycled along paths.}
}
\description{
These functions provide a low-level interface to the computer's file system.
}
\examples{
updateDirFilePermissions(getwd(), recursive = TRUE, mode = "0777")

}

