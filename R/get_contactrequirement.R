#' Retrieve a specific contact requirement from the Semestry API
#'
#' This function sends a GET request to the contact requirement endpoint of the Semestry API and retrieves data for a specific contact requirement.
#'
#' @param semestry An authenticated Semestry object.
#' @param contactrequirement_code The contact requirement code to retrieve data for.
#' @param timeout The timeout duration for the GET request (default: 30 seconds).
#'
#' @return The retrieved contact requirement data from the API.
#'
#' @export
get_contactrequirement <- function(semestry, contactrequirement_code, timeout = 30) {
  endpoint <- paste0("/v1/api/contactrequirement/", contactrequirement_code)

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
    stop(paste("Error: Failed to retrieve contact requirement", contactrequirement_code, "from the API. Status code:", httr::http_status(resp)$status_code))
  }
}