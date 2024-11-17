#' Summarize Electricity Statistics
#' 
#' This function will give summary statistics for the current state of electricity prices. It will take in a dataframe of locational marginal prices (that has been pulled from an API) and return the average price around NE-ISO, the standard deviation in prices, the lowest price, the highest price, and the locations for those prices. 
#' @param df A dataframe of locational marginal prices
#' @return A list of summary statistics
#' @examples
#' summarize(df)
#' @export
summarize_lmps = function(df){
  # Calculate the average price
  avg_price = mean(df$price)
  
  # Calculate the standard deviation in prices
  sd_price = sd(df$price)
  
  # Find the lowest price
  min_price = min(df$price)
  min_loc = df$location[df$price == min_price]
  
  # Find the highest price
  max_price = max(df$price)
  max_loc = df$location[df$price == max_price]
  
  # Return the summary statistics
  return(list("Average Price" = avg_price, 
              "Standard Deviation" = sd_price, 
              "Lowest Price" = min_price, 
              "Lowest Price Location" = min_loc, 
              "Highest Price" = max_price, 
              "Highest Price Location" = max_loc))
}