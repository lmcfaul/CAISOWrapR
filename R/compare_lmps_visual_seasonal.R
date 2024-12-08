#' Compare LMPs between two cities visually by Season
#' 
#' This script reads in two datasets containing LMPs for two different cities over a year (or other specified time period) and compares the distribution of LMPs by hour of the day by season
#' 
#' @param df1 A tibble containing LMP data for the first city over a year
#' @param df2 A tibble containing LMP data for the second city over a year
#' 
#' @return A ggplot object displaying the distribution of LMPs by hour of the day for the two cities by season
#' 
#' @import dplyr
#' @import ggplot2
#' 
#' @export
compare_lmps_visual_seasonal = function(df1, df2){
  df1 <- df1 %>%
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
    )
  
  df2 <- df2 %>%
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
    )
  
  avg_price_per_time1 <- df1 %>%
    filter(time_only == paste0(sprintf("%02d", hour), ":00:00")) %>% 
    group_by(hour, season) %>%
    summarise(
      lower_whisker = quantile(lmp, 0.1, na.rm = TRUE),
      lower_quartile = quantile(lmp, 0.25, na.rm = TRUE),
      median = median(lmp, na.rm = TRUE),
      upper_quartile = quantile(lmp, 0.75, na.rm = TRUE),
      upper_whisker = quantile(lmp, 0.9, na.rm = TRUE),
      city = pull(df1, city)[1]
    )
  
  avg_price_per_time2 <- df2 %>%
    filter(time_only == paste0(sprintf("%02d", hour), ":00:00")) %>% 
    group_by(hour, season) %>%
    summarise(
      lower_whisker = quantile(lmp, 0.1, na.rm = TRUE),
      lower_quartile = quantile(lmp, 0.25, na.rm = TRUE),
      median = median(lmp, na.rm = TRUE),
      upper_quartile = quantile(lmp, 0.75, na.rm = TRUE),
      upper_whisker = quantile(lmp, 0.9, na.rm = TRUE),
      city = pull(df2, city)[1]
    )
  
  city1 <- unique(avg_price_per_time1$city)
  city2 <- unique(avg_price_per_time2$city)
  
  # Plot
  q <- ggplot() +
    geom_rect(
      data = avg_price_per_time1,
      aes(
        xmin = hour - 0.35,
        xmax = hour - 0.005,
        ymin = lower_quartile,
        ymax = upper_quartile,
        fill = city
      ),
      alpha = 0.5
    ) +
    geom_segment(
      data = avg_price_per_time1,
      aes(
        x = hour - 0.2,
        xend = hour - 0.2,
        y = lower_whisker,
        yend = lower_quartile,
      ),
      color = "black"
    ) +
    geom_segment(
      data = avg_price_per_time1,
      aes(
        x = hour - 0.2,
        xend = hour - 0.2,
        y = upper_quartile,
        yend = upper_whisker,
      ),
      color = "black"
    ) +
    geom_point(
      data = avg_price_per_time1,
      aes(
        x = (hour - 0.35 + hour - 0.005) / 2,
        y = median,
      ),
      size = 0.75,
      color = "black"
    ) +
    geom_rect(
      data = avg_price_per_time2,
      aes(
        xmin = hour + 0.05,
        xmax = hour + 0.35,
        ymin = lower_quartile,
        ymax = upper_quartile,
        fill = city
      ),
      alpha = 0.5
    ) +
    geom_segment(
      data = avg_price_per_time2,
      aes(
        x = hour + 0.2,
        xend = hour + 0.2,
        y = lower_whisker,
        yend = lower_quartile,
      ),
      color = "black"
    ) +
    geom_segment(
      data = avg_price_per_time2,
      aes(
        x = hour + 0.2,
        xend = hour + 0.2,
        y = upper_quartile,
        yend = upper_whisker,
      ),
      color = "black"
    ) +
    geom_point(
      data = avg_price_per_time2,
      aes(
        x = hour + 0.2,
        y = median,
      ),
      color = "black",
      size = 0.75
    ) +
    scale_x_continuous(
      name = "Hour of the Day",
      breaks = seq(0, 23, 2),
      labels = sprintf("%02d:00", seq(0, 23, 2))
    ) +
    scale_fill_manual(
      name = "City",
      values = c("steelblue", "red")
    ) +
    scale_color_manual(
      name = "City",
      values = c("steelblue", "red")
    ) +
    labs(
      title = paste("LMP Distribution by Hour:", city1, "vs.", city2),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Locational Marginal Price ($/MWh)"
    ) +
    facet_wrap(~season, scales = "fixed", ncol = 2) + # Facet by season
    theme_solarized() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5)
    )
  
  q
  
}

