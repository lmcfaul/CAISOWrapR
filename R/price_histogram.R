#' Price Histogram
#' 
#' This script generates a histogram of the price of the LMPs in the dataset.
#' @param df A dataframe of locational marginal prices with LMP names
#' @param bins An integer of the number of bins to use in the histogram
#' @return A histogram of the prices
#' @export
#' @examples
#' price_histogram(df, 30)
#' @importFrom ggplot2 ggplot aes geom_histogram
price_histogram = function(df, bins) {
  
  # Create a histogram of the prices
  p = ggplot(df, aes(x = price)) +
    geom_histogram(bins = bins, fill = "blue", color = "black") +
    labs(title = "Histogram of LMP Prices", x = "Price", y = "Frequency")
  
  return(p)
}