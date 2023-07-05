#' Retrieve room data from an API
#'
#' This function sends a GET request to the rooms endpoint of a specified API and retrieves the room data.
#'
#' @param base_url The base URL of the API.
#' @param key The API key for authorization.
#' @param timeout The timeout duration for the GET request (default: 30 seconds).
#'
#' @return The retrieved room data from the API.
#'
#' @export
get_rooms <- function(base_url, key, timeout = 30) {

  endpoint <- "rooms"

  url <- paste0(base_url, endpoint)

  resp <- httr::GET(
    url,
    httr::add_headers("Authorization" = paste0("k", key)),
    httr::timeout(timeout)
  )

  if (resp$status_code == 200) {
    content <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content)
    return(data)
  } else {
    stop(paste("Error: Failed to retrieve room data from the API. Status code:", httr::http_status(resp)$status_code))
  }
}
