#' Price Violin
#' 
#' This script generates a violin plot of the LMP prices in the dataset, 
#' with an overlaid boxplot showing the median, interquartile range, and whiskers.
#' @param df A dataframe of locational marginal prices with LMP names
#' @return A violin plot of the prices with an overlaid boxplot
#' @importFrom ggplot2 ggplot aes geom_violin geom_boxplot labs theme element_blank
#' @import ggthemes
#' @import lubridate
#' @export
#' @examples
#' price_violin(df)
price_violin <- function(df) {
  
  # Load necessary library
  library(lubridate)
  
  # Extract and format the interval start time
  interval_time <- unique(df$interval_start_utc)[1]
  formatted_time <- format(ymd_hms(interval_time), "%Y-%m-%d at %H:%M")
  
  # Create a violin plot with an overlaid boxplot
  p <- ggplot(df, aes(x = "", y = lmp)) +
    geom_violin(fill = "gray", color = "black", alpha = 0.7) +
    geom_boxplot(width = 0.2, fill = "#E8E8E8", color = "black", outlier.shape = NA) +  # Add a boxplot
    labs(
      title = paste("Frequency of California Locational Marginal Prices on", formatted_time),
      x = "Frequency",
      y = "Price"
    ) +
    theme(
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank()
    ) +
    theme_solarized()  # Optional: Apply Solarized theme
  
  return(p)
}
