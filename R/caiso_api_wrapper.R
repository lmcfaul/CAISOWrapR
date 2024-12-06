#' ISONE API Wrapper Constructor
#'
#' Construct an CAISOClient object for interacting with the Grid Status API.
#' @param host The base URL of the Grid Status API. Default is "https://api.gridstatus.io/v1".
#' @param api_key The API key to use for authenticating requests to the API. If not provided, the function will
#' attempt to read the API key from the `GRIDSTATUS_API_KEY` environment variable.
#' @param max_retries The maximum number of times to retry a request if it fails. Default is 3.
#' @details
#' The CAISOClient object is used to interact with the Grid Status API. In order to fetch LMP data
#' from the API, need to generate a free api key from the Grid Status website. You can do so by
#' signing up for an account at https://gridstatus.io/signup. Once you have an API key, you can
#' pass it to the CAISOClient constructor or set the environment variable `GRIDSTATUS_API_KEY` to 
#' create a client object that can be used to fetch data.
#' @return An CAISOClient object.
#' @export CAISOClient
CAISOClient <- function(host = "https://api.gridstatus.io/v1",
                        api_key = NULL,
                        max_retries = 3) {
  
    if (is.null(api_key)) {
      api_key <- Sys.getenv("GRIDSTATUS_API_KEY", unset = "")
    }
    
    if (api_key == "") {
      stop("API key not provided. Either pass api_key to the constructor
           or define the GRIDSTATUS_API_KEY environment variable.")
    }
  
  structure(list(
    host = host,
    api_key = api_key,
    retries = max_retries
    ),
    class = "CAISOClient"
  )
}

#' Fetch data from the Grid Status API
#' 
#' Fetch data from the Grid Status API using the provided client, endpoint, and query parameters.
#' @param client An CAISOClient object created with \code{\link{CAISOClient}}.
#' @param endpoint The API endpoint to fetch data from.
#' @param params A list of query parameters to include in the request.
#' @return A list representing the JSON response from the API.
fetch_data <- function(client, endpoint, params = NULL) {
  # Check if params is NULL and set it to an empty list if so
  if (is.null(params)) {
    params <- list()
  }
  
  # Add default query parameters
  params$return_format <- "json"
  params$json_schema <- "array-of-arrays"
  
  # Define the headers for the request
  headers <- list(
    "x-api-key" = client$api_key
  )
  
  # Construct the base request and add query parameters
  request <- httr2::request(client$host) |> # Pipe the base URL into the request function
    httr2::req_url_path_append(endpoint) |> # Append the endpoint to the URL
    httr2::req_headers(!!!headers) |> # Unpack the headers list
    httr2::req_url_query(!!!params) |> # Unpack the parameters list
    httr2::req_retry(client$retries) |> # Set the number of retries
    httr2::req_timeout(10) # Set the request timeout
  
  # Send the request and return the response
  response <- httr2::req_perform(request)
  
  # Check the status code and stop if it's not 200
  if (response$status_code != 200) {
    stop("Request failed with status code ", response$status_code)
  }
  
  # Parse the response body as JSON
  return(httr2::resp_body_json(response))
}

#' Fetch LMP data from the Grid Status API
#' 
#' Fetch locational marginal price (LMP) data from the Grid Status API.
#' @param client An CAISOClient object created with \code{\link{CAISOClient}}.
#' @param dataset The name of the dataset to fetch data from. Default is "caiso_lmp_real_time_5_min".
#' Options are
#' @param start_tm The start time for the data query. If not provided, the API will use the earliest available time.
#' @param end_tm The end time for the data query. If not provided, the API will use the latest available time.
#' @param columns A character vector of column names to include in the response. If not provided, all columns will be returned.
#' @param filter_column The name of the column to filter on. If provided, the filter_value and filter_operator parameters must also be provided.
#' @param filter_value The value to filter on. If provided, the filter_column and filter_operator parameters must also be provided.
#' If filter operator is "in", filter value should be a list of values to filter on.
#' @param filter_operator The operator to use for the filter. Options are "=", ">", "<", ">=", "<=", "in". If not provided, the default is "=".
#' @param publish_tm Controls the filtering on the dataset's publish time. The possible values are as follows:
#' - "latest_report": Returns records only from the most recently published report
#' - "latest": For any given timestamp, returns the most recently published record associated with it
#' - timestamp str: Returns records that were published at the timestamp provided
#' - None: No filtering
#' @param resample The frequency to resample the data to. Options are 
#' @param resample_by A character vector of column names to group by when resampling the data.
#' By default resamples by the time index column. If resample is not provided, this parameter is ignored.
#' @param resample_function The function to use when resampling the data. Options are "mean", "sum", "min", "max", "stddev", "count", "variance".
#' Default is "mean".
#' @param limit The maximum number of records to return. Default is 5.
#' @param page_size The number of records to return per page. If not provided, the API will use the default page size allowed by subscription
#' @param tz The timezone to convert the timestamps to. Default is "UTC".
#' @param verbose If set to TRUE or "info", prints additional information. If set to "debug", prints more additional 
#' debug information. If set to FALSE, does not print any additional information. Default is FALSE.
#' @param use_cursor_pagination A logical value indicating whether to use cursor-based pagination. Default is FALSE.
#' @return A data frame containing the LMP data.
#' @export get_lmp.CAISOClient
get_lmp.CAISOClient <- function(client, dataset = "caiso_lmp_real_time_5_min", start_tm = NULL, 
                                end_tm = NULL, columns = NULL, filter_column = NULL, 
                                filter_value = NULL, filter_operator = NULL, publish_tm = NULL, 
                                resample = NULL, resample_by = NULL, resample_function = "mean", 
                                limit = 5, page_size = NULL, tz = NULL, verbose = FALSE, 
                                use_cursor_pagination = TRUE) {
  
  # Check if the start and end times are provided and convert them to the desired timezone
  if (is.null(tz)) {
    tz <- "UTC"
  }
  
  if (!is.null(start_tm)) {
    start_tm <- as.POSIXct(start_tm, tz = "UTC") # Parse the UTC timestamp
    start_tm <- format(start_tm, tz = tz, usetz = TRUE) # Convert to the desired timezone
  }
  
  if (!is.null(end_tm)) {
    end_tm <- as.POSIXct(end_tm, tz = "UTC") # Parse the UTC timestamp
    end_tm <- format(end_tm, tz = tz, usetz = TRUE) # Convert to the desired timezone
  }
  
  # Handle pagination
  page = 1
  next_page = TRUE
  
  # Initialize cursor
  cursor = ""
  
  # Initialize list to store data frames
  dfs <- list()
  
  while (next_page) {
    # Define the query parameters for the request
    params <- list(
      "start" = start_tm,
      "end" = end_tm,
      "limit" = limit,
      "page" = page,
      "page_size" = page_size,
      "resample_frequency" = resample,
      "resample_by" = if (!is.null(resample_by)) paste(resample_by, collapse = ",") else NULL,
      "resample_function" = if (!is.null(resample)) resample_function else NULL,
      "publish_time" = publish_tm
    )
    
    # Add cursor parameter if given
    if (use_cursor_pagination) {
      params$cursor <- cursor
    }
    
    #  Add filter parameters if given
    if (!is.null(filter_column) || !is.null(filter_value)) {
        if (is.list(filter_value) && filter_operator == "in") {
          filter_value <- paste(filter_value, collapse = ",")
        }
      
      params$filter_column <- filter_column
      params$filter_value <- filter_value
      params$filter_operator <- filter_operator
      
    }
    
    # Add columns parameter if given
    if (!is.null(columns)) {
      params$columns <- paste(columns, collapse = ",")
    }
    
    # Create endpoint for request
    endpoint <- paste("/datasets/", dataset, "/query", sep = "")
    
    # Fetch data from the API
    response <- fetch_data(client, endpoint, params = params)
    
    # Extract column names from the first element of response$data
    column_names <- response$data[[1]]
    
    # Extract the remaining data (skipping the first element)
    data_rows <- response$data[-1]
    
    # Convert the list of rows to a data frame, assigning column names
    df <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
    colnames(df) <- column_names
    
    # Convert numeric columns from character (if applicable)
    df[] <- lapply(df, function(x) if (suppressWarnings(all(!is.na(as.numeric(x))))) as.numeric(x) else x)
    
    # Append the data frame to the list of data frames
    dfs[[length(dfs) + 1]] <- df
    
    # Check if there is a next page
    next_page = if (!is.null(response$meta$next_page)) response$meta$next_page else FALSE
    
    # Extract cursor for pagination
    cursor = response$meta$cursor
    
    # Increment the page number
    page = page + 1
  }
  
  # Combine and return the final data framm
  final_df <- dplyr::bind_rows(dfs)
  return(final_df)
}
