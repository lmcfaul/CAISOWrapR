#' Pull Data from One Node
#' 
#' This function takes in a city or a node and a year and returns a dataframe of the price every 15 minutes for that year in that location. Default is Palo Alto for 2023.
#' 
#' @param city City name (automatically set to NULL).
#' @param node Node name (automatically set to NULL).
#' @param year Year (2020-2023).
#' @param api_key API key for the CAISO API.
#' 
#' @return A dataframe of the price every 15 minutes for that year in that location.
#' 
#' @importFrom lubridate as.POSIXct as.POSIXlt as.Date
#' 
#' @export
#' 
#' @examples
#' pulldata_node(city = "San Diego", year = 2020, api_key = api_key)
pulldata_node <- function(city = NULL, node = NULL, year = 2023, api_key) {
  
  valid_years <- c(2020, 2021, 2022, 2023)
  if (!(year %in% valid_years)) {
    stop("Please provide a valid year (2020, 2021, 2022, or 2023).")
  }
  
  if (is.null(node) && is.null(city)) {
    city <- "Palo Alto"
  }
  current_date <- as.character(Sys.Date() - 2)
  
  if (!is.null(city)) {
    city_lmps <- read.csv("inst/extdata/cities_and_lmps.csv")  # Load city-node mapping
    node <- city_lmps$closest_lmp[city_lmps$name == city]
    node <- as.character(node[1])
    if (length(node) == 0) {
      stop("City not found in the cities_and_lmps.csv file.")
    }
  }
  
  if (!is.null(node)) {
    city <- city_lmps$name[city_lmps$closest_lmp == node]
    city <- as.character(city[1])
    if (length(city) == 0 || is.na(city)) {
      stop("Node not found in the cities_and_lmps.csv file.")
    }
  }
  
  # Define the 2-month chunks
  start_dates <- c(
    paste(year, "-01-01", sep = ""), paste(year, "-03-01", sep = ""), 
    paste(year, "-05-01", sep = ""), paste(year, "-07-01", sep = ""), 
    paste(year, "-09-01", sep = ""), paste(year, "-11-01", sep = "")
  )
  
  end_dates <- c(
    paste(year, "-03-01", sep = ""), paste(year, "-05-01", sep = ""), 
    paste(year, "-07-01", sep = ""), paste(year, "-09-01", sep = ""), 
    paste(year, "-11-01", sep = ""), paste(year, "-12-31", sep = "")
  )
  
  for (i in seq_along(start_dates)) {
    if (as.Date(start_dates[i]) > as.Date(current_date)) {
      start_dates <- start_dates[1:(i - 1)]
      end_dates <- end_dates[1:(i - 1)]
      break
    }
    if (as.Date(end_dates[i]) > as.Date(current_date)) {
      end_dates[i] <- current_date
    }
  }
  
  all_data <- list()  # To store data from each chunk
  client_pull <- create_caiso_client(api_key = api_key)
  
  for (i in seq_along(start_dates)) {
    start_time_iso <- paste(start_dates[i], " 08:00:00", sep = "")
    end_time_iso <- paste(end_dates[i], " 08:00:00", sep = "")
    
    df_node <- fetch_lmp_data(
      client = client_pull,
      start_time = start_time_iso,
      end_time = end_time_iso,
      filter_column = "location",
      filter_value = node,
      filter_operator = "=",
      limit = 5840  # Adjust limit as needed
    )
    df_node$city <- city
    all_data[[i]] <- df_node
  }
  
  for (i in seq_along(all_data)) {
    if ("interval_start_utc" %in% names(all_data[[i]])) {
      if (is.list(all_data[[i]]$interval_start_utc)) {
        date_time_strings <- unlist(all_data[[i]]$interval_start_utc)
        all_data[[i]]$interval_start_utc <- as.POSIXct(date_time_strings, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
      } else if (is.character(all_data[[i]]$interval_start_utc)) {
        all_data[[i]]$interval_start_utc <- as.POSIXct(all_data[[i]]$interval_start_utc, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
      }
    }
    if ("interval_end_utc" %in% names(all_data[[i]])) {
      if (is.list(all_data[[i]]$interval_end_utc)) {
        date_time_strings <- unlist(all_data[[i]]$interval_end_utc)
        all_data[[i]]$interval_end_utc <- as.POSIXct(date_time_strings, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
      } else if (is.character(all_data[[i]]$interval_end_utc)) {
        all_data[[i]]$interval_end_utc <- as.POSIXct(all_data[[i]]$interval_end_utc, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
      }
    }
  }
  
  final_data <- do.call(rbind, all_data)
  return(final_data)
}
