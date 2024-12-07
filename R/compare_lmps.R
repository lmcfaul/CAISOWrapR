#' Compare LMPs
#' 
#' Function gets the input of two locations and a dataframe of locational marginal prices and returns the price comparison between the two locations.
#' @param df A dataframe of locational marginal prices with LMP names
#' @param loc1 A string of the first location
#' @param loc2 A string of the second location
#' @return A named vector of the two locations and the difference in price
#' @export
#' @examples
#' compare_lmps(df, "San Francisco", "San Diego")
#' @importFrom dplyr filter
compare_lmps = function(df, loc1, loc2) {
  
  cities_and_lmps = read.csv("data/cities_and_lmps.csv")
  
  # Check if both locations exist in the cities_and_lmps data
  if (!(loc1 %in% cities_and_lmps$name)) {
    return(paste("Location", loc1, "is not in the dataset. Please try another location."))
  }
  if (!(loc2 %in% cities_and_lmps$name)) {
    return(paste("Location", loc2, "is not in the dataset. Please try another location."))
  }
  
  # Lookup the LMP names from location_and_lmps based on loc1 and loc2
  lmp1 = cities_and_lmps$closest_lmp[cities_and_lmps$name == loc1]
  lmp2 = cities_and_lmps$closest_lmp[cities_and_lmps$name == loc2]
  
  # Find the price for each LMP name in df
  price1 = df$lmp[df$location == lmp1]
  price2 = df$lmp[df$location == lmp2]
  
  # Calculate the difference in price
  diff_price = price1 - price2
  
  # Return a named vector
  price_comparison = c(loc1 = price1, loc2 = price2, difference = diff_price)
  return(price_comparison)
}

