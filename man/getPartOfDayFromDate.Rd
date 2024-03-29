% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/featureEngineering.R
\name{getPartOfDayFromDate}
\alias{getPartOfDayFromDate}
\title{Get the Part of Day for a particular Date}
\usage{
getPartOfDayFromDate(date)
}
\description{
This is used for the extractDateFeatures default function for retrieving the part of day. This is currently using a user-specified time range. Can be modified and substituted
for use in the extractDateFeatures if required.
}
\details{
It will assume the following mapping:
 5am -< 12pm Morning
 12pm -< 5pm Afternoon
 5pm -< 9pm Evening
 9pm -< 5am Night
 @param date A date containing a vector of dates to compute the part of day for.
 @return The part of day associated to each date (Morning, Afternoon, Evening or Night).
 @examples
 getPartOfDayFromDate(Sys.Date())
 @export
}

