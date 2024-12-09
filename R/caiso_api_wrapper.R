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
#' Retrieve LMP data with essential querying options.
#'
#' @param client A GridStatusClient object created with \code{\link{create_caiso_client}}
#' @param dataset Name of the CAISO LMP dataset to query. (default: "caiso_lmp_real_time_15_min")
#'   Options:
#'   - "caiso_lmp_real_time_15_min": 15-minute real-time LMP data
#'   - "caiso_lmp_real_time_5_min": 5-minute real-time LMP data
#'   - "caiso_lmp_day_ahead_hourly": Hourly day-ahead LMP data
#'
#' @return A data frame containing the retrieved LMP data
#'
#' @examples
#' \dontrun{
#' # Create a client
#' client <- create_caiso_client(api_key = "your_api_key")
#'
#' # Basic usage
#' lmp_data <- fetch_lmp_data(client)
#' }
#'
#' @section Advanced Query Options (Future Use):
#' The \code{fetch_lmp_data} function currently supports essential parameters, but may support additional features in the future. These advanced options are aligned with the capabilities of the Grid Status API and are available for custom extensions. The following options are planned for future inclusion:
#' 
#' - **Time-based Filtering**:
#'   - \code{start_time} and \code{end_time} for retrieving data over specific intervals.
#'   - Formats: \code{"YYYY-MM-DD HH:MM:SS"} or \code{"YYYY-MM-DD"} (automatically converted to UTC).
#' 
#' - **Filtering and Selection**:
#'   - \code{filter_column}, \code{filter_value}, and \code{filter_operator} for applying custom filters on data.
#'     - \code{filter_column}: The column to apply the filter on (e.g., "location").
#'     - \code{filter_value}: The value to match against in the selected column.
#'     - \code{filter_operator}: Supported operators include:
#'       - \code{"="} (exact match)
#'       - \code{">"} (greater than)
#'       - \code{"<"} (less than)
#'       - \code{">="} (greater than or equal to)
#'       - \code{"<="} (less than or equal to)
#'       - \code{"in"} (multiple value matching).
#'   - \code{columns} to specify which columns to retrieve from the dataset.
#'
#' - **Resampling and Aggregation**:
#'   - \code{resample_frequency} for specifying the frequency at which to resample the data (e.g., "1H" for hourly).
#'   - \code{resample_by} for grouping the data when resampling (e.g., "location", "market").
#'   - \code{resample_function} for the aggregation method when resampling:
#'     - \code{"mean"}, \code{"sum"}, \code{"min"}, \code{"max"}, \code{"stddev"}, \code{"count"}, \code{"variance"}.
#'
#' - **Time Zone Customization**:
#'   - \code{tz} to specify the time zone for timestamp conversion. This would adjust timestamps according to the local time zone, instead of always using UTC.
#'
#' - **Pagination**:
#'   - \code{page_size} and \code{limit} to control pagination:
#'     - \code{page_size}: Number of records per page (default is the API’s value).
#'     - \code{limit}: Maximum number of records to return.
#'   - Supports cursor-based pagination or traditional pagination, depending on the API response.
#'
#' These advanced options are not currently implemented but are intended for future use. Users may extend the function themselves or refer to the API documentation for more detailed customization.
#'
#' @seealso \code{\link{create_caiso_client}}
#'
#' @export
fetch_lmp_data <- function(
    client, 
    dataset = "caiso_lmp_real_time_15_min", 
    ...
) {
  # Default timezone
  tz <- "UTC"
  
  # Validate input parameters
  args <- list(...)
  
  # Check if a custom timezone is provided in the arguments
  if (!is.null(args$tz)) {
    tz <- args$tz  # Override the default timezone
    args$tz <- NULL  # Remove from args to avoid sending it to the API
  }
  
  # Handle start_time and end_time conversions
  if (!is.null(args$start_time)) {
    args$start_time <- as.POSIXct(args$start_time, tz = tz, format = "%Y-%m-%d %H:%M:%S")
    args$start_time <- format(args$start_time, tz = "UTC", usetz = FALSE)  # Convert to UTC for API
  }
  
  if (!is.null(args$end_time)) {
    args$end_time <- as.POSIXct(args$end_time, tz = tz, format = "%Y-%m-%d %H:%M:%S")
    args$end_time <- format(args$end_time, tz = "UTC", usetz = FALSE)  # Convert to UTC for API
  }
  
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
  
  # Check if there are any data rows
  if (length(response$data) == 1) {
    # No data rows, return an empty data frame with appropriate column names
    empty_df <- as.data.frame(matrix(ncol = length(column_names), nrow = 0))
    colnames(empty_df) <- column_names
    
    # Apply type conversion to ensure correct data types
    empty_df <- convert_lmp_datatypes(empty_df)
    
    return(empty_df)
  }
  
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
#   start = "2024-01-01", 
#   end = "2024-01-02",
#   limit = 10,
#   columns = c("interval_start_utc", "lmp")
# )