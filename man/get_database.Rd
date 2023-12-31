% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_database.R
\name{get_database}
\alias{get_database}
\title{Retrieve data from the Semestry API database endpoint}
\usage{
get_database(semestry, timeout = 30)
}
\arguments{
\item{semestry}{An authenticated Semestry object.}

\item{timeout}{The timeout duration for the GET request (default: 30 seconds).}
}
\value{
The retrieved data from the API database.
}
\description{
This function sends a GET request to the database endpoint of the Semestry API and retrieves the data.
}
