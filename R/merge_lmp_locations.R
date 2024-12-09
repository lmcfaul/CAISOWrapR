#' Takes instance dataframe and merges with location and latitude longitude data
#' 
#' This function takes an instance dataframe (with LMP data) and merges it with location data 
#' (including city names and corresponding latitude and longitude), returning a dataframe with 
#' both the LMP and location information.
#' 
#' @param instance A dataframe containing the LMP instance data to be merged with location data.
#' 
#' @return A dataframe containing both the LMP instance data and the location information.
#' 
#' @examples
#' # Example usage of the merge_lmp_locations function
#' merge_lmp_locations(instance)
#' 
#' @export
merge_lmp_locations <- function(instance) {
  # Load the location data
  location_data <- read.csv("inst/extdata/cities_and_lmps.csv")
  
  # Merge the instance data with the location data
  merged_data <- instance %>%
    dplyr::left_join(
      location_data %>% dplyr::distinct(closest_lmp, .keep_all = TRUE),
      by = c("location" = "closest_lmp")
    )
  
  # Clean up the merged data by renaming columns and removing unnecessary ones
  merged_data <- merged_data %>%
    dplyr::rename(city = name) %>%
    dplyr::select(-closest_distance)
  
  return(merged_data)
}
