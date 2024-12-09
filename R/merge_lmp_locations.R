#' Takes instance dataframe and merges with location and latitude longitude data
#' 
#' @param instance dataframe of instance data
#' 
#' @return dataframe with instance data and location data
#' 
#' @examples
#' merge_lmp_locations(instance)
#' 
#' @import dplyr
#' 
#' @export
merge_lmp_locations <- function(instance) {
  # Load the location data
  location_data <- read.csv(system.file("extdata", "cities_and_lmps.csv", package = "CAISOWrapR"))
  
  # Merge the instance data with the location data
  merged_data <- instance %>%
    left_join(
      location_data %>% distinct(closest_lmp, .keep_all = TRUE),
      by = c("location" = "closest_lmp")
    )
  merged_data <- merged_data %>%
    rename(city = name) %>%
    select(-closest_distance)
  
  return(merged_data)
}
