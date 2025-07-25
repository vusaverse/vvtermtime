#' Retrieve a specific building from the Semestry API
#'
#' This function sends a GET request to the building endpoint of the Semestry API and retrieves data for a specific building.
#'
#' @param semestry An authenticated Semestry object.
#' @param building_code The building code to retrieve data for.
#' @param campus_code The campus code for the building.
#' @param timeout The timeout duration for the GET request (default: 30 seconds).
#'
#' @return The retrieved building data from the API.
#'
#' @export
get_building <- function(semestry, building_code, campus_code, timeout = 30) {
  endpoint <- paste0("/v1/api/building/", building_code, "/", campus_code)

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
    stop(paste("Error: Failed to retrieve building", building_code, "on campus", campus_code, "from the API. Status code:", httr::http_status(resp)$status_code))
  }
}