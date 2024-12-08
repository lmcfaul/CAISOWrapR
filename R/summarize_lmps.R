#' Summarize Electricity Statistics
#' 
#' This function will give summary statistics for the current state of electricity prices. It will take in a dataframe of locational marginal prices (that has been pulled from an API) and return the average price around NE-ISO, the standard deviation in prices, the lowest price, the highest price, and the locations for those prices. 
#' @param df A dataframe of locational marginal prices
#' @return A list of summary statistics
#' @examples
#' summarize_lmps(df)
#' @export
summarize_lmps = function(df){
  # Calculate the average price
  avg_price = mean(df$lmp)
  
  # Calculate the standard deviation in prices
  sd_price = sd(df$lmp)
  
  cities_and_lmps = read.csv("data/cities_and_lmps.csv")
  
  # Find the lowest price
  min_price = min(df$lmp)
  min_loc_lmp = df$location[df$lmp == min_price]
  min_loc_city = cities_and_lmps$name[cities_and_lmps$closest_lmp == min_loc_lmp]
  
  # Find the highest price
  max_price = max(df$lmp)
  max_loc_lmp = df$location[df$lmp == max_price]
  max_loc_city = cities_and_lmps$name[cities_and_lmps$closest_lmp == max_loc_lmp]
  
  summary_df = data.frame(
    Statistic = c("Average Price", "Standard Deviation", "Lowest Price", 
                  "Lowest Price Location", "Highest Price", "Highest Price Location"),
    Value = c(
      avg_price, 
      sd_price, 
      min_price, 
      paste(min_loc_city, collapse = ", "), 
      max_price, 
      paste(max_loc_city, collapse = ", ")
    ),
    stringsAsFactors = FALSE
  )
  
  # Set row names to the Statistic column
  rownames(summary_df) <- summary_df$Statistic
  summary_df$Statistic <- NULL
  
  return(summary_df)
}
