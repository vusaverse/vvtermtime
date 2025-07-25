% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_timetable_student.R
\name{get_timetable_student}
\alias{get_timetable_student}
\title{Retrieve timetable data for a specific student from the Semestry API}
\usage{
get_timetable_student(
  semestry,
  student_code,
  start = NULL,
  end = NULL,
  timeout = 30
)
}
\arguments{
\item{semestry}{An authenticated Semestry object.}

\item{student_code}{The student code to retrieve timetable data for.}

\item{start}{Optional start date in YYYY-MM-DD format (10 characters).}

\item{end}{Optional end date in YYYY-MM-DD format (10 characters).}

\item{timeout}{The timeout duration for the GET request (default: 30 seconds).}
}
\value{
The retrieved timetable data for the specified student from the API.
}
\description{
This function sends a GET request to the timetable student endpoint of the Semestry API and retrieves the timetable data for a specific student.
}
