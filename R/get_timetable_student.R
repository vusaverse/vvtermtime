#' Retrieve timetable data for a specific student from the Semestry API
#'
#' This function sends a GET request to the timetable student endpoint of the Semestry API and retrieves the timetable data for a specific student.
#'
#' @param semestry An authenticated Semestry object.
#' @param student_code The student code to retrieve timetable data for.
#' @param start Optional start date in YYYY-MM-DD format (10 characters).
#' @param end Optional end date in YYYY-MM-DD format (10 characters).
#' @param timeout The timeout duration for the GET request (default: 30 seconds).
#'
#' @return The retrieved timetable data for the specified student from the API.
#'
#' @export
get_timetable_student <- function(semestry, student_code, start = NULL, end = NULL, timeout = 30) {
  endpoint <- paste0("/v1/api/timetable/student/", student_code)

  # Build query parameters
  query_params <- list()
  if (!is.null(start)) {
    query_params$start <- start
  }
  if (!is.null(end)) {
    query_params$end <- end
  }

  url <- paste0(semestry$base_url, endpoint)
  if (length(query_params) > 0) {
    query_string <- paste(names(query_params), query_params, sep = "=", collapse = "&")
    url <- paste0(url, "?", query_string)
  }

  resp <- httr::GET(
    url,
    httr::add_headers("Authorization" = paste0("k", semestry$api_key)),
    httr::timeout(timeout)
  )

  if (resp$status_code == 200) {
    content <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content)
    return(data)
  } else {
    stop(paste("Error: Failed to retrieve timetable data for student", student_code, "from the API. Status code:", httr::http_status(resp)$status_code))
  }
}
