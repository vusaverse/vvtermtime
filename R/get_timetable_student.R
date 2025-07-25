#' Retrieve timetable data for a specific student from the Semestry API
#'
#' This function sends a GET request to the timetable student endpoint of the Semestry API and retrieves the timetable data for a specific student.
#'
#' @param semestry An authenticated Semestry object.
#' @param student_code The student code to retrieve timetable data for.
#' @param timeout The timeout duration for the GET request (default: 30 seconds).
#'
#' @return The retrieved timetable data for the specified student from the API.
#'
#' @export
get_timetable_student <- function(semestry, student_code, timeout = 30) {
  endpoint <- paste0("/v1/api/timetable/student/", student_code)

  url <- paste0(semestry$base_url, endpoint)

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