#' Compare LMPs
#' 
#' Function gets the input of two locations and a dataframe of locational marginal prices and returns the price comparison between the two locations.
#' @param df A dataframe of locational marginal prices with LMP names
#' @param loc1 A string of the first location
#' @param loc2 A string of the second location
#' @return A named vector of the two locations and the difference in price
#' @export
#' @examples
#' compare_lmps(df, "Boston", "New York")
#' @importFrom dplyr filter
compare_lmps = function(df, loc1, loc2) {
  # Lookup the LMP names from location_and_lmps based on loc1 and loc2
  lmp1 = location_and_lmps$lmp_name[location_and_lmps$location == loc1]
  lmp2 = location_and_lmps$lmp_name[location_and_lmps$location == loc2]
  
  # Find the price for each LMP name in df
  price1 = df$price[df$lmp_name == lmp1]
  price2 = df$price[df$lmp_name == lmp2]
  
  # Calculate the difference in price
  diff_price = price1 - price2
  
  # Return a named vector
  price_comparison = c(loc1 = price1, loc2 = price2, difference = diff_price)
  return(price_comparison)
}

