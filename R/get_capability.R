#' Retrieve a specific capability from the Semestry API
#'
#' This function sends a GET request to the capability endpoint of the Semestry API and retrieves data for a specific capability.
#'
#' @param semestry An authenticated Semestry object.
#' @param capability_code The capability code to retrieve data for.
#' @param timeout The timeout duration for the GET request (default: 30 seconds).
#'
#' @return The retrieved capability data from the API.
#'
#' @export
get_capability <- function(semestry, capability_code, timeout = 30) {
  endpoint <- paste0("/v1/api/capability/", capability_code)

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
    stop(paste("Error: Failed to retrieve capability", capability_code, "from the API. Status code:", httr::http_status(resp)$status_code))
  }
}