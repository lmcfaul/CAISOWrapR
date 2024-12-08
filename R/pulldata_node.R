#' Pull Data from One Node
#' 
#' This function takes in a city or a node and a year and returns a dataframe of the price every 15 minutes for that year in that location
#' 
#' @param city city (automatically set to NULL)
#' @param node node (automatically set to NULL)
#' @param year year (2020-2024)
#' 
#' @return a dataframe of the price every 15 minutes for that year in that location
#' 
#' @import lubridate
#' 
#' @export
#' 
#' @examples
#' pulldata_node(city = "San Diego", year = 2020)
pulldata_node = function(city = NULL, node = NULL, year = 2020, api_key){
  
  #print(list(city = city, node = node, year = year))
  
  # Check if city is provided, if so, look up the node in cities_and_lmps.csv
  if(!is.null(city)){
    city_lmps = read.csv("data/cities_and_lmps.csv")  # Assuming this CSV contains city names and corresponding LMP nodes
    node = city_lmps$closest_lmp[city_lmps$name == city]
    node <- as.character(node[1])
    #print(str(node))
    # Check if the city is found in the CSV
    if(length(node) == 0){
      stop("City not found in the cities_and_lmps.csv file.")
    }
  }
  
  if (!is.null(node)) {
    city = city_lmps$name[city_lmps$closest_lmp == node]
    city <- as.character(city[1])
    if (length(city) == 0 || is.na(city)) {
      stop("Node not found in the cities_and_lmps.csv file.")
    }
  } else {
    stop("Either a city or a node must be provided.")
  }

  # Define the 2-month chunks
  start_dates <- seq(from = as.Date(paste(year, "-01-01", sep = "")), 
                     to = as.Date(paste(year, "-11-01", sep = "")), by = "2 months")
  end_dates <- c(start_dates[-1], as.Date(paste(year + 1, "-01-01", sep = ""))) - 1
  
  all_data <- list()  # To store data from each chunk
  
  client_pull = CAISOClient(api_key = api_key)
  
  for (i in seq_along(start_dates)) {
    start_time_iso <- format(start_dates[i], "%Y-%m-%d %H:%M:%S")
    end_time_iso <- format(end_dates[i], "%Y-%m-%d %H:%M:%S")
    
    #print(start_time_iso)
    #print(end_time_iso)
    
    # Fetch LMP data for the given node and time range
    df_node <- get_lmp.CAISOClient(
      client = client_pull,
      start_tm = start_time_iso,
      end_tm = end_time_iso,
      filter_column = "location",
      filter_value = node,
      filter_operator = "=",
      limit = 5840  # Adjust limit as needed
    )
    
    df_node$city <- city
    
    #print(paste("Fetched data from", start_time_iso, "to", end_time_iso))
    #print(class(df_node))
    #print(str(df_node))
    
    all_data[[i]] <- df_node
  }
  
  # Combine all chunks into a single data frame
  final_data <- do.call(rbind, all_data)
  
  return(final_data)
}
