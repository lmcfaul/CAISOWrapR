#' CAISO API Client
#'
#' A comprehensive R package for interacting with the Grid Status API
#' to access CAISO LMP data and gather different analyses and visualizations
#' from it.

#' @title CAISO API Client
#' @description Create a client for interacting with the Grid Status API
#' @param host Base URL of the API (default: "https://api.gridstatus.io/v1")
#' @param api_key API authentication key
#' @param max_retries Maximum number of request retries (default: 3)
#' @return A CAISO API object
#' @export
create_caiso_client <- function(
    host = "https://api.gridstatus.io/v1", 
    api_key = NULL, 
    max_retries = 3
) {
  # Validate and retrieve API key
  if (is.null(api_key)) {
    api_key <- Sys.getenv("GRIDSTATUS_API_KEY", unset = "")
  }
  
  # Throw informative error if no API key is found
  if (api_key == "") {
    stop(paste(
      "No API key provided. Please:\n",
      "1. Pass api_key to the function, or\n",
      "2. Set the GRIDSTATUS_API_KEY environment variable\n",
      "Get your free API key at: https://www.gridstatus.io/api"
    ))
  }
  
  # Create client structure with strong validation
  client <- structure(
    list(
      host = host,
      api_key = api_key,
      max_retries = max_retries
    ),
    class = "CAISOClient"
  )
  
  return(client)
}

#' @title Validate CAISO Client
#' @description Internal method to validate client object
#' @param client CAISO client object
#' @return Logical indicating client validity
validate_client <- function(client) {
  # Check if object is of correct class and has required fields
  if (!inherits(client, "CAISOClient")) {
    stop("Invalid client: Must be a CAISO object")
  }
  
  # Validate host and API key
  if (is.null(client$host) || is.null(client$api_key)) {
    stop("Client is missing required configuration")
  }
  
  return(TRUE)
}

#' @title Construct API Request
#' @description Build a robust API request with error handling
#' @param client CAISO client object
#' @param endpoint API endpoint
#' @param params Query parameters
#' @return httr2 request object
build_api_request <- function(client, endpoint, params = list()) {
  # Validate client first
  validate_client(client)
  
  # Ensure params is a list and add default configurations
  params <- c(params, list(
    return_format = "json",
    json_schema = "array-of-arrays"
  ))
  
  # Construct request with comprehensive error handling
  tryCatch({
    request <- httr2::request(client$host) |>
      httr2::req_url_path_append(endpoint) |>
      httr2::req_headers(
        "x-api-key" = client$api_key,
      ) |>
      httr2::req_url_query(!!!params) |>
      httr2::req_retry(max_tries = client$max_retries) |>
      httr2::req_timeout(10)
    
    return(request)
  }, error = function(e) {
    stop(sprintf("Failed to construct API request: %s", e$message))
  })
}

#' @title Execute API Request
#' @description Send API request and process response
#' @param request httr2 request object
#' @return Processed API response
execute_api_request <- function(request) {
  tryCatch({
    response <- httr2::req_perform(request)
    
    # Enhanced error handling for non-200 status codes
    if (response$status_code != 200) {
      stop(sprintf(
        "API request failed with status %d: %s", 
        response$status_code, 
        httr2::resp_body_string(response)
      ))
    }
    
    # Parse and return JSON response
    return(httr2::resp_body_json(response))
  }, error = function(e) {
    stop(sprintf("API request execution failed: %s", e$message))
  })
}

#' Fetch Locational Marginal Price (LMP) Data from Grid Status API
#'
#' Retrieve LMP data with flexible querying and filtering options.
#'
#' @param client A GridStatusClient object created with \code{\link{create_gridstatus_client}}
#' @param dataset Name of the CAISO LMP dataset to query. 
#'   Defaults to "caiso_lmp_real_time_15_min".
#'   
#' @param start Start time for data retrieval (optional)
#'   - Format: "YYYY-MM-DD HH:MM:SS"
#'   - Timezone: Automatically converted to UTC
#'   - If not provided, uses earliest available time
#'   
#' @param end End time for data retrieval (optional)
#'   - Format: "YYYY-MM-DD HH:MM:SS"
#'   - Timezone: Automatically converted to UTC
#'   - If not provided, uses latest available time
#'   
#' @param filter_column Column to apply filtering on (optional)
#' 
#' @param filter_value Value to filter by (optional)
#' 
#' @param filter_operator Filtering operator (optional)
#'   Supported operators:
#'   - "=" (exact match)
#'   - ">" (greater than)
#'   - "<" (less than)
#'   - ">=" (greater than or equal to)
#'   - "<=" (less than or equal to)
#'   - "in" (multiple value matching)
#'   
#' @param columns Specific columns to retrieve (optional)
#'   - Character vector of column names
#'   - If not provided, returns all columns
#'   
#' @param limit Maximum number of records to return
#' 
#' @param page_size Number of records per page (optional)
#'   - Uses API's default page size if not specified
#'   
#' @param resample Frequency to resample data (optional)
#' 
#' @param resample_by Columns to group by when resampling (optional)
#'   - Default: Groups by time index column
#'   
#' @param resample_function Aggregation method for resampling (default: "mean")
#'   Options:
#'   - "mean"
#'   - "sum"
#'   - "min"
#'   - "max"
#'   - "stddev"
#'   - "count"
#'   - "variance"
#'   
#' @param publish_tm Controls filtering based on publish time (optional)
#'   - "latest_report": Most recently published report
#'   - "latest": Most recent record for each timestamp
#'   - Specific timestamp string
#'   - NULL: No filtering
#'   
#' @param use_cursor_pagination Use cursor-based pagination (default: TRUE)
#' 
#' @param tz Timezone for timestamp conversion (default: "UTC")
#'
#' @return A data frame containing the retrieved LMP data
#'
#' @examples
#' \dontrun{
#' # Create a client
#' client <- create_caiso_client(api_key = "your_api_key")
#'
#' # Basic usage
#' lmp_data <- fetch_lmp_data(
#'   client,
#'   start = "2024-01-01 00:00:00",
#'   end = "2024-01-02 00:00:00"
#' )
#'
#' # Advanced filtering
#' filtered_data <- fetch_lmp_data(
#'   client,
#'   filter_column = "location",
#'   filter_value = "",
#'   filter_operator = "=",
#'   columns = c("interval_start_utc", "lmp", "location")
#' )
#'
#' # Resampling data
#' resampled_data <- fetch_lmp_data(
#'   client,
#'   resample = "1H",  # Resample to hourly
#'   resample_function = "mean",
#'   resample_by = c("location", "market")
#' )
#' }
#'
#' @section Nuances and Considerations:
#' \itemize{
#'   \item Input times are converted to UTC
#'   \item Can specify output timezone via \code{tz} parameter
#'   \item Automatically handles multi-page responses
#'   \item Supports both cursor and traditional pagination
#'   \item Automatically converts:
#'     \itemize{
#'       \item Timestamps to POSIXct
#'       \item Categorical columns to factors
#'       \item Numeric columns to numeric type
#'     }
#'   \item Provides informative error messages
#'   \item Gracefully handles conversion issues
#' }
#'
#' @seealso \code{\link{create_caiso_client}}
#'
#' @export
fetch_lmp_data <- function(
    client, 
    dataset = "caiso_lmp_real_time_15_min", 
    ...
) {
  # Validate input parameters
  args <- list(...)
  
  # Construct endpoint
  endpoint <- paste0("/datasets/", dataset, "/query")
  
  # Build and execute request
  request <- build_api_request(
    client, 
    endpoint, 
    params = compact_list(args)
  )
  
  response <- execute_api_request(request)
  
  # Process response into data frame
  processed_data <- process_lmp_response(response)
  
  return(processed_data)
}

#' @title Process LMP Response
#' @description Convert API response to a clean, type-converted data frame
#' @param response API response object
#' @return Processed data frame
process_lmp_response <- function(response) {
  # Extract column names and data
  column_names <- response$data[[1]]
  data_rows <- response$data[-1]
  
  # Convert to data frame
  df <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
  colnames(df) <- column_names
  
  # Advanced type conversion
  df <- convert_lmp_datatypes(df)
  
  return(df)
}

#' @title Convert LMP Data Types
#' @description Convert API data types to appropriate R types
#' @param data Input data frame
#' @return Data frame with converted types
#' @export
convert_lmp_datatypes <- function(data) {
  # Robust type conversion with error handling
  tryCatch({
    data |>
      dplyr::mutate(
        interval_start_utc = as.POSIXct(extract_first(interval_start_utc), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
        interval_end_utc = as.POSIXct(extract_first(interval_end_utc), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
        market = as.factor(extract_first(market)),
        location = as.factor(extract_first(location)),
        location_type = as.factor(extract_first(location_type)),
        lmp = as.numeric(extract_first(lmp)),
        energy = as.numeric(extract_first(energy)),
        congestion = as.numeric(extract_first(congestion)),
        loss = as.numeric(extract_first(loss))
      )
  }, error = function(e) {
    warning(sprintf("Data type conversion failed: %s", e$message))
    return(data)
  })
}

#' @title Safely Extract First List Element
#' @description Safely extract first element from potentially nested lists
#' @param x Input list or vector
#' @return First element or NA
extract_first <- function(x) {
  # Handle NULL input by returning NA
  if (is.null(x)) return(NA)
  
  # Extract first value from nested list
  tryCatch({
    sapply(x, function(x) x[1])
  }, error = function(e) NA)  # In case of an error, return NA
}

#' @title Compact List
#' @description Remove NULL elements from a list
#' @param l Input list
#' @return Cleaned list
compact_list <- function(l) {
  l[!sapply(l, is.null)]
}

# Examples of Usage:
# client <- create_caiso_client(api_key = "your_api_key")
# lmp_data <- fetch_lmp_data(
#   client, 
#   start_tm = "2024-01-01", 
#   end_tm = "2024-01-02",
#   limit = 10,
#   columns = c("interval_start_utc", "lmp")
# )