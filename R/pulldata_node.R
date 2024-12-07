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
#' @export
#' 
#' @examples
#' pulldata_node(city = "San Diego", year = 2020)
pulldata_node = function(city = NULL, node = NULL, year = 2020, api_key){
  
  # Check if city is provided, if so, look up the node in cities_and_lmps.csv
  if(!is.null(city)){
    city_lmps = read.csv("data/cities_and_lmps.csv")  # Assuming this CSV contains city names and corresponding LMP nodes
    node = city_lmps$node[city_lmps$city == city]
    
    # Check if the city is found in the CSV
    if(length(node) == 0){
      stop("City not found in the cities_and_lmps.csv file.")
    }
  }
  
  # If node is provided, proceed with the data pull for that node
  if(!is.null(node)){
    start_time = ymd(paste(year, "-01-01"))
    end_time = ymd(paste(year + 1, "-01-01"))
    
    start_time_iso = format(start_time, "%Y-%m-%d %H:%M:%S")  
    end_time_iso = format(end_time, "%Y-%m-%d %H:%M:%S")  
    
    unique_lmps = read.csv("data/lmps.csv")  # Assuming this contains the LMP node names
    client_pull = CAISOClient(api_key = api_key)
    
    # Fetch LMP data for the given node and year
    df_instance = get_lmp.CAISOClient(
      client = client_pull,
      start_tm = start_time_iso,
      end_tm = end_time_iso,
      filter_column = "location",
      filter_value = node,
      filter_operator = "in",
      limit = 100000  # You can adjust the limit as necessary
    )
    
    return(df_instance)
  } else {
    stop("Either a city or a node must be provided.")
  }
}