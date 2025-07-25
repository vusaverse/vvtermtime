#' Retrieve a specific activity template from the Semestry API
#'
#' This function sends a GET request to the activity template endpoint of the Semestry API and retrieves data for a specific activity template.
#'
#' @param semestry An authenticated Semestry object.
#' @param activitytemplate_code The activity template code to retrieve data for.
#' @param fields Optional parameter to specify which fields to return.
#' @param format Optional parameter to specify the response format.
#' @param timeout The timeout duration for the GET request (default: 30 seconds).
#'
#' @return The retrieved activity template data from the API.
#'
#' @export
get_activitytemplate <- function(semestry, activitytemplate_code, fields = NULL, format = NULL, timeout = 30) {
  endpoint <- paste0("/v1/api/activitytemplate/", activitytemplate_code)

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
    stop(paste("Error: Failed to retrieve activity template", activitytemplate_code, "from the API. Status code:", httr::http_status(resp)$status_code))
  }
}