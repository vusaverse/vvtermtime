% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_activity_types.R
\name{get_activity_types}
\alias{get_activity_types}
\title{Retrieve activity type data from the Semestry API}
\usage{
get_activity_types(semestry, timeout = 30)
}
\arguments{
\item{semestry}{An authenticated Semestry object.}

\item{timeout}{The timeout duration for the GET request (default: 30 seconds).}
}
\value{
The retrieved activity type data from the API.
}
\description{
This function sends a GET request to the activity types endpoint of the Semestry API and retrieves the activity type data.
}
