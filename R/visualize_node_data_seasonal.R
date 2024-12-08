#' Visualize distribution of LMPs in one location by season
#' 
#' This script reads in a dataframe of LMP data for a city over a year and visualizes the distribution of LMPs by hour for each season.
#' 
#' @param df A tibble containing LMP data for the first city over a year
#' 
#' @return A ggplot object displaying the distribution of LMPs by hour for the location over a day
#' 
#' @import dplyr
#' @import ggplot2
#' @import lubridate with_tz
#' @import ggthemes theme_solarized
#' 
#' @export
visualize_node_seasonal = function(df){
  
  df <- df %>%
    mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S"),
      hour = as.numeric(substr(time_only, 1, 2)),
      month = as.numeric(format(interval_start_pst, "%m")),
      season = case_when(
        month %in% c(12, 1, 2) ~ "Winter",
        month %in% c(3, 4, 5) ~ "Spring",
        month %in% c(6, 7, 8) ~ "Summer",
        month %in% c(9, 10, 11) ~ "Fall",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(season)) # Remove rows with NA seasons
  
  avg_price_per_time <- df %>%
    group_by(hour, season) %>%
    summarise(
      lower_whisker = quantile(lmp, 0.1, na.rm = TRUE),
      lower_quartile = quantile(lmp, 0.25, na.rm = TRUE),
      median = median(lmp, na.rm = TRUE),
      upper_quartile = quantile(lmp, 0.75, na.rm = TRUE),
      upper_whisker = quantile(lmp, 0.9, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Define breaks and labels for x-axis
  #x_breaks <- unique(avg_price_per_time$hour)
  #x_labels <- x_breaks[as.numeric(substr(x_breaks, 1, 2)) %% 2 == 0]
  
  x_breaks <- seq(0, 23, by = 2)
  x_labels <- sprintf("%02d:00", x_breaks)
  
  # Plot
  p <- ggplot(avg_price_per_time, aes(x = hour)) +
    geom_rect(
      aes(
        xmin = as.numeric(factor(hour)) - 0.4,
        xmax = as.numeric(factor(hour)) + 0.4,
        ymin = lower_quartile,
        ymax = upper_quartile
      ),
      fill = "steelblue",
      alpha = 0.5
    ) +
    geom_segment(
      aes(
        x = as.numeric(factor(hour)),
        xend = as.numeric(factor(hour)),
        y = lower_whisker,
        yend = lower_quartile
      ),
      color = "black"
    ) +
    geom_segment(
      aes(
        x = as.numeric(factor(hour)),
        xend = as.numeric(factor(hour)),
        y = upper_quartile,
        yend = upper_whisker
      ),
      color = "black"
    ) +
    geom_point(aes(
      x = as.numeric(factor(hour)),
      y = median
    ), color = "black", size = 0.75) +
    scale_x_continuous(
      name = "Hour of the Day",
      breaks = x_breaks,
      labels = x_labels
    ) +
    labs(
      title = "Seasonal LMP Distribution by Hour",
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Locational Marginal Price ($/MWh)",
      fill = "Median LMP"
    ) +
    facet_wrap(~season, scales = "fixed", ncol = 2) + # Facet by season
    theme_solarized() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5)
    )
  
  p
  
}