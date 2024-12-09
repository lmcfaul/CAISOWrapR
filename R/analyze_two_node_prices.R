# This script has all of the functions that analyze two nodes of data (two locations with all 15 minute intervals for a year for each) and combines the results into a pdf to export. The functions `compare_lmps_visual` and `compare_lmps_visual_seasonal` are called with 2 inputting dataframes of a node of data. The user has to call the `pulldata_node` function for two different cities/nodes to get the data for the nodes they want to analyze. The user can also just call the individual analysis functions if they want to see the results separately.

#' Analyze Two Node Data
#' 
#' This function calls the functions `compare_lmps_visual` and `compare_lmps_visual_seasonal` with two inputting dataframes of two nodes of data (two locations over a year) and combines the results into a pdf to export. First, the user has to call the `pulldata_node` function twice to get the data for the two nodes they want to analyze.
#' 
#' @param df1 A dataframe of locational marginal prices for a year for one location
#' @param df2 A dataframe of locational marginal prices for a year for another location
#' @param output_pdf_path The path to save the pdf file. There is a default path of "single_instance_data.pdf" but the user can edit this path to save the file in a different location.
#' 
#' @return A pdf file with the two plots
#' 
#' @import grid
#' @import gridExtra
#' @import ggplot2
#' @import lubridate
#' @import gtable
#' 
#' @export
analyze_two_node_data <- function(df1, df2, output_pdf_path = "two_node_data.pdf") {
  result_one <- compare_lmps_visual(df1, df2)
  result_two <- compare_lmps_visual_seasonal(df1, df2)
  
  interval_time <- unique(df1$interval_start_utc)[1]
  year_of_data <- format(ymd_hms(interval_time), "%Y")
  
  title <- textGrob(
    paste("LMP Distributions for", df1$city[1], "and", df2$city[1], "in", year_of_data),
    gp = gpar(fontsize = 16, fontface = "bold"),
    just = "center"
  )
  
  plot_with_margins_1 <- gtable_add_padding(
    ggplotGrob(result_one),
    padding = unit(c(1, 1, 1, 1), "cm")
  )
  
  plot_with_margins_2 <- gtable_add_padding(
    ggplotGrob(result_two),
    padding = unit(c(1, 1, 1, 1), "cm")
  )
  
  pdf(output_pdf_path, width = 8.5, height = 11)
  
  grid.arrange(
    title,
    plot_with_margins_1,
    plot_with_margins_2,
    nrow = 3,
    heights = c(0.1, 0.45, 0.45)
  )
  
  dev.off()
  
  message("Results saved to: ", output_pdf_path)
}


#' Compare LMPs between two cities visually
#' 
#' This script reads in two datasets containing LMPs for two different cities over a year (or other specified time period) and compares the distribution of LMPs by hour of the day.
#' 
#' @param df1 A tibble containing LMP data for the first city over a year
#' @param df2 A tibble containing LMP data for the second city over a year
#' 
#' @return A ggplot object displaying the distribution of LMPs by hour of the day for the two cities
#' 
#' @import dplyr
#' @import ggplot2
#' 
#' @export
compare_lmps_visual = function(df1, df2){
  df1 <- df1 %>%
    mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S"),
      hour = as.numeric(substr(time_only, 1, 2))
    )
  
  df2 <- df2 %>%
    mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S"),
      hour = as.numeric(substr(time_only, 1, 2))
    )
  
  avg_price_per_time1 <- df1 %>%
    filter(time_only == paste0(sprintf("%02d", hour), ":00:00")) %>% 
    group_by(hour) %>%
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
    group_by(hour) %>%
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
    theme_solarized() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5)
    )
  
  return(q)
  
}



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
  
  return(q)
  
}

