#' Retrieve floor data from the Semestry API
#'
#' This function sends a GET request to the floors endpoint of the Semestry API and retrieves the floor data.
#'
#' @param semestry An authenticated Semestry object.
#' @param timeout The timeout duration for the GET request (default: 30 seconds).
#'
#' @return The retrieved floor data from the API.
#'
#' @export
get_floors <- function(semestry, timeout = 30) {
  endpoint <- "/v1/api/floors"

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
    stop(paste("Error: Failed to retrieve floor data from the API. Status code:", httr::http_status(resp)$status_code))
  }
}
