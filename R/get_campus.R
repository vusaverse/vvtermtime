#' Retrieve a specific campus from the Semestry API
#'
#' This function sends a GET request to the campus endpoint of the Semestry API and retrieves data for a specific campus.
#'
#' @param semestry An authenticated Semestry object.
#' @param campus_code The campus code to retrieve data for.
#' @param timeout The timeout duration for the GET request (default: 30 seconds).
#'
#' @return The retrieved campus data from the API.
#'
#' @export
get_campus <- function(semestry, campus_code, timeout = 30) {
  endpoint <- paste0("/v1/api/campus/", campus_code)

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
    stop(paste("Error: Failed to retrieve campus", campus_code, "from the API. Status code:", httr::http_status(resp)$status_code))
  }
}