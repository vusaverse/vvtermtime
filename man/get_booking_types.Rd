% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_booking_types.R
\name{get_booking_types}
\alias{get_booking_types}
\title{Retrieve booking type data from the Semestry API}
\usage{
get_booking_types(semestry, timeout = 30)
}
\arguments{
\item{semestry}{An authenticated Semestry object.}

\item{timeout}{The timeout duration for the GET request (default: 30 seconds).}
}
\value{
The retrieved booking type data from the API.
}
\description{
This function sends a GET request to the booking types endpoint of the Semestry API and retrieves the booking type data.
}
