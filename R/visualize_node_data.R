#' Visualize Node Data
#' 
#' This function takes in a dataframe of a year's worth of 15-minute data for a node
#' and visualizes the data in a box and whisker plot by interval to see how the price distribution ranges
#' over a day
#' 
#' @param df a dataframe of a year's worth of 15-minute data for a node
#' 
#' @return 1 graph
#' 
#' @import ggplot2
#' @import dplyr
#' @import lubridate with_tz
#' @import ggthemes theme_solarized
#' 
#' @export
visualize_node = function(df){
  
  df <- df %>%
    mutate(
      # Convert to POSIXct in UTC
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      # Convert to PST (Pacific Time)
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      # Extract time-only portion in PST
      time_only = format(interval_start_pst, "%H:%M:%S")
    )
  
  avg_price_per_time <- df %>%
    group_by(time_only) %>%
    summarise(
      lower_whisker = quantile(lmp, 0.1, na.rm = TRUE),
      lower_quartile = quantile(lmp, 0.25, na.rm = TRUE),
      median = median(lmp, na.rm = TRUE),
      upper_quartile = quantile(lmp, 0.75, na.rm = TRUE),
      upper_whisker = quantile(lmp, 0.9, na.rm = TRUE)
    )
  
  p <- ggplot(avg_price_per_time, aes(x = time_only)) +
    # Draw the interquartile range as a "box"
    geom_rect(
      aes(
        xmin = as.numeric(factor(time_only)) - 0.4,
        xmax = as.numeric(factor(time_only)) + 0.4,
        ymin = lower_quartile,
        ymax = upper_quartile,
      ),
      fill = "steelblue",
      alpha = 0.5
    ) +
    # Draw whiskers for 5th and 95th percentiles
    geom_segment(
      aes(
        x = as.numeric(factor(time_only)),
        xend = as.numeric(factor(time_only)),
        y = lower_whisker,
        yend = lower_quartile
      ),
      color = "black"
    ) +
    geom_segment(
      aes(
        x = as.numeric(factor(time_only)),
        xend = as.numeric(factor(time_only)),
        y = upper_quartile,
        yend = upper_whisker
      ),
      color = "black"
    ) +
    # Add points for the median
    geom_point(aes(
      x = as.numeric(factor(time_only)),
      y = median
    ), color = "black", size = .75) +
    # Add labels and theme
    scale_x_discrete(
      name = "Hour of the Day",
      breaks = avg_price_per_time$time_only[avg_price_per_time$time_only %in% sprintf("%02d:00:00", seq(0, 22, 2))], # Filter specific times
      labels = sprintf("%02d:00", seq(0, 22, 2))  # Display in HH:MM format
    ) +
    labs(
      title = "LMP Distribution by 15-minute Interval",
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Locational Marginal Price ($/MWh)",
      fill = "Median LMP"
    ) +
    theme_solarized() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  p
  
}