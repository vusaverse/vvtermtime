#' Authenticate with Semestry Termtime API
#'
#' This function authenticates with the Semestry Termtime API by verifying the API key and base URL.
#' It returns an authenticated Semestry object that can be used for subsequent API calls.
#'
#' @param api_key The API key for Semestry.
#' @param base_url The base URL of the Semestry API.
#'
#' @return An authenticated Semestry object.
#'
#' @export
authenticate <- function(api_key, base_url) {
  # Create a Semestry object to store the API key and base URL
  semestry <- list(api_key = api_key, base_url = base_url)

  # Check if the base URL ends with a trailing slash and remove it if present
  if (substr(semestry$base_url, nchar(semestry$base_url), nchar(semestry$base_url)) == "/") {
    semestry$base_url <- substr(semestry$base_url, 1, nchar(semestry$base_url) - 1)
  }

  # Verify authentication by making a test request
  test_url <- paste0(semestry$base_url, "/v1/api/rooms")
  response <- httr::GET(test_url, httr::add_headers("Authorization" = paste0("k", semestry$api_key)))

  # Check the response status code
  if (response$status_code != 200) {
    stop("Authentication failed. Please check your API key and base URL.")
  }

  # Return the Semestry object
  return(semestry)
}
