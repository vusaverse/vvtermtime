#' Ping the Semestry API
#'
#' This function sends a GET request to the ping endpoint of the Semestry API to check connectivity.
#'
#' @param semestry An authenticated Semestry object.
#' @param timeout The timeout duration for the GET request (default: 30 seconds).
#'
#' @return The ping response from the API.
#'
#' @export
get_ping <- function(semestry, timeout = 30) {
  endpoint <- "/v1/api/ping"

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
    stop(paste("Error: Failed to ping API. Status code:", httr::http_status(resp)$status_code))
  }
}