#' Retrieve a specific activity type from the Semestry API
#'
#' This function sends a GET request to the activity type endpoint of the Semestry API and retrieves data for a specific activity type.
#'
#' @param semestry An authenticated Semestry object.
#' @param activitytype_code The activity type code to retrieve data for.
#' @param fields Optional parameter to specify which fields to return.
#' @param format Optional parameter to specify the response format.
#' @param timeout The timeout duration for the GET request (default: 30 seconds).
#'
#' @return The retrieved activity type data from the API.
#'
#' @export
get_activitytype <- function(semestry, activitytype_code, fields = NULL, format = NULL, timeout = 30) {
  endpoint <- paste0("/v1/api/activitytype/", activitytype_code)

  # Build query parameters
  query_params <- list()
  if (!is.null(fields)) {
    query_params$fields <- fields
  }
  if (!is.null(format)) {
    query_params$format <- format
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
    stop(paste("Error: Failed to retrieve activity type", activitytype_code, "from the API. Status code:", httr::http_status(resp)$status_code))
  }
}