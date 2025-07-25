#' Retrieve timetable data for a specific module from the Semestry API
#'
#' This function sends a GET request to the timetable module endpoint of the Semestry API and retrieves the timetable data for a specific module.
#'
#' @param semestry An authenticated Semestry object.
#' @param module_code The module code to retrieve timetable data for.
#' @param timeout The timeout duration for the GET request (default: 30 seconds).
#'
#' @return The retrieved timetable data for the specified module from the API.
#'
#' @export
get_timetable_module <- function(semestry, module_code, timeout = 30) {
  endpoint <- paste0("/v1/api/timetable/module/", module_code)

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
    stop(paste("Error: Failed to retrieve timetable data for module", module_code, "from the API. Status code:", httr::http_status(resp)$status_code))
  }
}