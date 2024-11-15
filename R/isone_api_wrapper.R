#' ISONE API Wrapper Constructor
#'
#' Construct an ISONEClient object for interacting with the ISO New England API.
#' @details
#' In order to utilize ISO New England's RESTFul interface, you need basic authentication credentials 
#' (username and password). To do so, set the environment variables `ISONE_API_USERNAME` and 
#' `ISONE_API_PASSWORD` to your username and password for your ISO New England, respectively. If you do not have a
#' username and password, you can create a free account for ISO New England by visiting the following website:
#' \url{https://www.iso-ne.com/isoexpress/login?p_p_id=58&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&saveLastPath=0&_58_struts_action=%2Flogin%2Fcreate_account}.
#' 
#' @return An ISONEClient object.
ISONEClient <- function() {
  username <- tryCatch(
    Sys.getenv("ISONE_API_USERNAME"),
    error = function(e) 
    stop("ISONE_API_USERNAME environment variable not set:", e$message)
  )
  
  password <- tryCatch(
    Sys.getenv("ISONE_API_PASSWORD"),
    error = function(e) 
    stop("ISONE_API_PASSWORD environment variable not set:", e$message)
  )
  
  structure(list(
    base_url = "https://webservices.iso-ne.com/api/v1.1",
    username = username,
    password = password
    ),
    class = "ISONEClient"
  )
}

#' Fetch data from the ISO New England API
#' 
#' Fetch data from the ISO New England API using the provided client, endpoint, and query parameters.
#' @param client An ISONEClient object created with \code{\link{ISONEClient}}.
#' @param endpoint The API endpoint to fetch data from.
#' @param params A list of query parameters to include in the request.
#' @return A list representing the JSON response from the API.
fetch_data <- function(client, endpoint, params = list()) {
  # Construct the base request and add query parameters
  request <- httr2::request(client$base_url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_auth_basic(username = client$username, password = client$password) |>
    httr2::req_query(!!!params)
  
  # Send the request and return the response
  response <- request |> httr2::req_perform()
  
  # Check the status code and stop if it's not 200
  if (response$status_code != 200) {
    stop("Request failed with status code ", response$status_code)
  }
  
  # Parse the response body as JSON
  return(httr2::resp_body_json(response))
}

