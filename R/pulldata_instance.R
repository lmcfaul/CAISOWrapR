#' Pull One Data Instnace
#' 
#' This function takes in a date and time (15 minute increment) and returns a dataframe of every LMP and the price breakdown
#' 
#' @param date Date in format "YYYY-MM-DD"
#' @param time Time in format "HH:MM"
#' @param api_key API key for the CAISO API
#' 
#' @return A dataframe with the LMP and price breakdown for the given date and time
#' 
#' @importFrom lubridate ymd_hms round_date minutes
#' 
#' @export
#' 
#' @examples
#' pulldata_instance("2019-01-01", "00:00", api_key = api_key)
pulldata_instance <- function(date, time, api_key) {
  
  adjust_to_nearest_15 <- function(time) {
    parsed_time <- lubridate::ymd_hms(time)
    rounded_time <- lubridate::round_date(parsed_time, unit = "15 minutes")
    return(rounded_time)
  }
  
  input_datetime <- paste(date, time, "00")
  input_datetime <- lubridate::ymd_hms(input_datetime)
  
  start_time <- adjust_to_nearest_15(input_datetime)
  start_time_iso <- format(start_time, "%Y-%m-%d %H:%M:%S")  # Remove the "T"
  print(start_time_iso)
  
  end_time <- start_time + lubridate::minutes(15)
  end_time_iso <- format(end_time, "%Y-%m-%d %H:%M:%S")  # Remove the "T"
  print(end_time_iso)
  
  unique_lmps <- read.csv("inst/extdata/lmps.csv")
  
  # Load the data
  client_pull <- create_caiso_client(api_key = api_key)
  
  df_instance <- fetch_lmp_data(
    client = client_pull,
    start_time = start_time_iso,
    end_time = end_time_iso,
    filter_column = "location",
    filter_value = paste(unique_lmps$unique_lmps, collapse = ","),
    filter_operator = "in",
    limit = length(unique_lmps$unique_lmps)
  )
  
  return(df_instance)
}
