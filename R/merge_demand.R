#' Merges a node with CAISO demand
#' 
#' This function merges a node with the CAISO demand data by interval_start_tm and adds column demand for demand data
#' 
#' @param node_data a data frame for a single node
#' 
#' @return a data frame with the node data and the demand data
#' @export
merge_demand <- function(node_data) {
  # Read CAISO demand data
  demand_data <- utils::read.csv(system.file("extdata", "caiso_load_2020_2023.csv", package = "CAISOWrapR"))
  demand_data <- demand_data %>%
    mutate(interval_start_utc = lubridate::ymd_hms(interval_start_utc, tz = "UTC"))
  # Merge node data with demand data
  
  node_data <- dplyr::left_join(
    node_data,
    dplyr::select(demand_data, interval_start_utc, load),
    by = "interval_start_utc"
  )
  
  return(node_data)
}
