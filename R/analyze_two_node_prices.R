# This script has all of the functions that analyze two nodes of data (two locations with all 15 minute intervals for a year for each) and combines the results into a pdf to export. The functions `compare_lmps_visual` and `compare_lmps_visual_seasonal` are called with 2 inputting dataframes of a node of data (same for that of congestion and loss) The user has to call the `pulldata_node` function for two different cities/nodes to get the data for the nodes they want to analyze. The user can also just call the individual analysis functions if they want to see the results separately.

#' Analyze Two Node Data
#' 
#' This function calls the functions `compare_lmps_visual`, `compare_lmps_visual_seasonal`, `compare_congestion_visual`, `compare_congestion_visual_seasonal`, `compare_losses_visual`, and `compare_losses_visual_seasonal` with two inputting dataframes of two nodes of data (two locations over a year) and combines the results into a pdf to export. First, the user has to call the `pulldata_node` function twice to get the data for the two nodes they want to analyze, or they can use the default.
#' 
#' @param df1 A dataframe of locational marginal prices for a year for one location. The default is San Francisco for 2023. Users can change this dataframe to analyze a different location or year by pulling from the `pulldata_node` function
#' @param df2 A dataframe of locational marginal prices for a year for another location. The defualt is Los Angeles for 2023. Users can change this dataframe to analyze a different location or year by pulling from the `pulldata_node` function
#' @param output_pdf_path The path to save the pdf file. There is a default path of "single_instance_data.pdf" but the user can edit this path to save the file in a different location.
#' 
#' @return A pdf file with the plots
#' 
#' @export
#analyze_two_node_data <- function(df1 = read.csv("inst/extdata/San_Francisco_node_2023.csv"), df2 = read.csv("inst/extdata/Los_Angeles_node_2023.csv"), output_pdf_path = "two_node_data.pdf") {
analyze_two_node_data <- function(df1 = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR")), df2 = read.csv(system.file("extdata", "Los_Angeles_node_2023.csv", package = "CAISOWrapR")), output_pdf_path = "two_node_data.pdf") {
  result_one <- compare_lmps_visual(df1, df2)
  result_two <- compare_lmps_visual_seasonal(df1, df2)
  result_three <- compare_congestion_visual(df1, df2)
  result_four <- compare_congestion_visual_seasonal(df1, df2)
  result_five <- compare_losses_visual(df1, df2)
  result_six <- compare_losses_visual_seasonal(df1, df2)
  
  interval_time <- unique(df1$interval_start_utc)[1]
  year_of_data <- format(lubridate::ymd_hms(interval_time), "%Y")
  
  title <- grid::textGrob(
    paste("LMP Distributions for", df1$city[1], "and", df2$city[1], "in", year_of_data),
    gp = grid::gpar(fontsize = 16, fontface = "bold"),
    just = "center"
  )
  
  plot_with_margins_1 <- gtable_add_padding1(
    ggplot2::ggplotGrob(result_one),
    padding = grid::unit(c(1, 1, 1, 1), "cm")
  )
  
  plot_with_margins_2 <- gtable_add_padding1(
    ggplot2::ggplotGrob(result_two),
    padding = grid::unit(c(1, 1, 1, 1), "cm")
  )
  
  plot_with_margins_3 <- gtable_add_padding1(
    ggplot2::ggplotGrob(result_three),
    padding = grid::unit(c(1, 1, 1, 1), "cm")
  )
  
  plot_with_margins_4 <- gtable_add_padding1(
    ggplot2::ggplotGrob(result_four),
    padding = grid::unit(c(1, 1, 1, 1), "cm")
  )
  
  plot_with_margins_5 <- gtable_add_padding1(
    ggplot2::ggplotGrob(result_five),
    padding = grid::unit(c(1, 1, 1, 1), "cm")
  )
  
  plot_with_margins_6 <- gtable_add_padding1(
    ggplot2::ggplotGrob(result_six),
    padding = grid::unit(c(1, 1, 1, 1), "cm")
  )
  
  pdf(output_pdf_path, width = 8.5, height = 11)
  
  gridExtra::grid.arrange(
    title,
    plot_with_margins_1,
    plot_with_margins_2,
    nrow = 3,
    heights = c(0.1, 0.45, 0.45)
  )
  
  gridExtra::grid.arrange(
    grid::textGrob(
      paste("Congestion for", df1$city[1], "and", df2$city[1], "in", year_of_data),
      gp = grid::gpar(fontsize = 16, fontface = "bold"),
      just = "center"
    ),
    plot_with_margins_3,
    plot_with_margins_4,
    nrow = 3,
    heights = c(0.1, 0.45, 0.45)
  )
  
  gridExtra::grid.arrange(
    grid::textGrob(
      paste("Loss Pricing Adjustments for", df1$city[1], "and", df2$city[1], "in", year_of_data),
      gp = grid::gpar(fontsize = 16, fontface = "bold"),
      just = "center"
    ),
    plot_with_margins_5,
    plot_with_margins_6,
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
##' @param df1 A dataframe of locational marginal prices for a year for one location. The default is San Francisco for 2023. Users can change this dataframe to analyze a different location or year by pulling from the `pulldata_node` function
#' @param df2 A dataframe of locational marginal prices for a year for another location. The defualt is Los Angeles for 2023. Users can change this dataframe to analyze a different location or year by pulling from the `pulldata_node` function
#' 
#' @return A ggplot object displaying the distribution of LMPs by hour of the day for the two cities
#' 
#' @export
compare_lmps_visual = function(df1 = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR")), df2 = read.csv(system.file("extdata", "Los_Angeles_node_2023.csv", package = "CAISOWrapR"))){
  df1 <- dplyr::mutate(
    df1,
    interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
    time_only = format(interval_start_pst, "%H:%M:%S"),
    hour = as.numeric(substr(time_only, 1, 2))
  )
  
  df2 <- dplyr::mutate(
    df2,
    interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
    time_only = format(interval_start_pst, "%H:%M:%S"),
    hour = as.numeric(substr(time_only, 1, 2))
  )
  
  avg_price_per_time1 <- dplyr::filter(df1, time_only == paste0(sprintf("%02d", hour), ":00:00")) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(
      lower_whisker = quantile(lmp, 0.1, na.rm = TRUE),
      lower_quartile = quantile(lmp, 0.25, na.rm = TRUE),
      median = median(lmp, na.rm = TRUE),
      upper_quartile = quantile(lmp, 0.75, na.rm = TRUE),
      upper_whisker = quantile(lmp, 0.9, na.rm = TRUE),
      city = dplyr::pull(df1, city)[1]
    )
  
  avg_price_per_time2 <- dplyr::filter(df2, time_only == paste0(sprintf("%02d", hour), ":00:00")) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(
      lower_whisker = quantile(lmp, 0.1, na.rm = TRUE),
      lower_quartile = quantile(lmp, 0.25, na.rm = TRUE),
      median = median(lmp, na.rm = TRUE),
      upper_quartile = quantile(lmp, 0.75, na.rm = TRUE),
      upper_whisker = quantile(lmp, 0.9, na.rm = TRUE),
      city = dplyr::pull(df2, city)[1]
    )
  
  city1 <- unique(avg_price_per_time1$city)
  city2 <- unique(avg_price_per_time2$city)
  
  # Plot
  q <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = avg_price_per_time1,
      ggplot2::aes(
        xmin = hour - 0.35,
        xmax = hour - 0.005,
        ymin = lower_quartile,
        ymax = upper_quartile,
        fill = city
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = hour - 0.2,
        xend = hour - 0.2,
        y = lower_whisker,
        yend = lower_quartile,
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = hour - 0.2,
        xend = hour - 0.2,
        y = upper_quartile,
        yend = upper_whisker,
      ),
      color = "black"
    ) +
    ggplot2::geom_point(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = (hour - 0.35 + hour - 0.005) / 2,
        y = median,
      ),
      size = 0.75,
      color = "black"
    ) +
    ggplot2::geom_rect(
      data = avg_price_per_time2,
      ggplot2::aes(
        xmin = hour + 0.05,
        xmax = hour + 0.35,
        ymin = lower_quartile,
        ymax = upper_quartile,
        fill = city
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        xend = hour + 0.2,
        y = lower_whisker,
        yend = lower_quartile,
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        xend = hour + 0.2,
        y = upper_quartile,
        yend = upper_whisker,
      ),
      color = "black"
    ) +
    ggplot2::geom_point(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        y = median,
      ),
      color = "black",
      size = 0.75
    ) +
    ggplot2::scale_x_continuous(
      name = "Hour of the Day",
      breaks = seq(0, 23, 2),
      labels = sprintf("%02d:00", seq(0, 23, 2))
    ) +
    ggplot2::scale_fill_manual(
      name = "City",
      values = c("steelblue", "red")
    ) +
    ggplot2::scale_color_manual(
      name = "City",
      values = c("steelblue", "red")
    ) +
    ggplot2::labs(
      title = paste("LMP Distribution by Hour:", df1$city[1], "vs.", df2$city[1]),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Locational Marginal Price ($/MWh)"
    ) +
    ggthemes::theme_solarized() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)
    )
  
  return(q)
  
}

#' Compare LMPs between two cities visually by Season
#' 
#' This script reads in two datasets containing LMPs for two different cities over a year (or other specified time period) and compares the distribution of LMPs by hour of the day by season
#' 
#' @param df1 A dataframe of locational marginal prices for a year for one location. The default is San Francisco for 2023. Users can change this dataframe to analyze a different location or year by pulling from the `pulldata_node` function
#' @param df2 A dataframe of locational marginal prices for a year for another location. The defualt is Los Angeles for 2023. Users can change this dataframe to analyze a different location or year by pulling from the `pulldata_node` function
#' 
#' @return A ggplot object displaying the distribution of LMPs by hour of the day for the two cities by season
#' 
#' @export
compare_lmps_visual_seasonal = function(df1 = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR")), df2 = read.csv(system.file("extdata", "Los_Angeles_node_2023.csv", package = "CAISOWrapR"))) {
  
  df1 <- df1 %>%
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S"),
      hour = as.numeric(substr(time_only, 1, 2)),
      month = as.numeric(format(interval_start_pst, "%m")),
      season = dplyr::case_when(
        month %in% c(12, 1, 2) ~ "Winter",
        month %in% c(3, 4, 5) ~ "Spring",
        month %in% c(6, 7, 8) ~ "Summer",
        month %in% c(9, 10, 11) ~ "Fall",
        TRUE ~ NA_character_
      )
    )
  
  df2 <- df2 %>%
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S"),
      hour = as.numeric(substr(time_only, 1, 2)),
      month = as.numeric(format(interval_start_pst, "%m")),
      season = dplyr::case_when(
        month %in% c(12, 1, 2) ~ "Winter",
        month %in% c(3, 4, 5) ~ "Spring",
        month %in% c(6, 7, 8) ~ "Summer",
        month %in% c(9, 10, 11) ~ "Fall",
        TRUE ~ NA_character_
      )
    )
  
  avg_price_per_time1 <- df1 %>%
    dplyr::filter(time_only == paste0(sprintf("%02d", hour), ":00:00")) %>%
    dplyr::group_by(hour, season) %>%
    dplyr::summarise(
      lower_whisker = quantile(lmp, 0.1, na.rm = TRUE),
      lower_quartile = quantile(lmp, 0.25, na.rm = TRUE),
      median = median(lmp, na.rm = TRUE),
      upper_quartile = quantile(lmp, 0.75, na.rm = TRUE),
      upper_whisker = quantile(lmp, 0.9, na.rm = TRUE),
      city = dplyr::pull(df1, city)[1]
    )
  
  avg_price_per_time2 <- df2 %>%
    dplyr::filter(time_only == paste0(sprintf("%02d", hour), ":00:00")) %>%
    dplyr::group_by(hour, season) %>%
    dplyr::summarise(
      lower_whisker = quantile(lmp, 0.1, na.rm = TRUE),
      lower_quartile = quantile(lmp, 0.25, na.rm = TRUE),
      median = median(lmp, na.rm = TRUE),
      upper_quartile = quantile(lmp, 0.75, na.rm = TRUE),
      upper_whisker = quantile(lmp, 0.9, na.rm = TRUE),
      city = dplyr::pull(df2, city)[1]
    )
  
  city1 <- unique(avg_price_per_time1$city)
  city2 <- unique(avg_price_per_time2$city)
  
  # Plot
  q <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = avg_price_per_time1,
      ggplot2::aes(
        xmin = hour - 0.35,
        xmax = hour - 0.005,
        ymin = lower_quartile,
        ymax = upper_quartile,
        fill = city
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = hour - 0.2,
        xend = hour - 0.2,
        y = lower_whisker,
        yend = lower_quartile
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = hour - 0.2,
        xend = hour - 0.2,
        y = upper_quartile,
        yend = upper_whisker
      ),
      color = "black"
    ) +
    ggplot2::geom_point(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = (hour - 0.35 + hour - 0.005) / 2,
        y = median
      ),
      size = 0.75,
      color = "black"
    ) +
    ggplot2::geom_rect(
      data = avg_price_per_time2,
      ggplot2::aes(
        xmin = hour + 0.05,
        xmax = hour + 0.35,
        ymin = lower_quartile,
        ymax = upper_quartile,
        fill = city
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        xend = hour + 0.2,
        y = lower_whisker,
        yend = lower_quartile
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        xend = hour + 0.2,
        y = upper_quartile,
        yend = upper_whisker
      ),
      color = "black"
    ) +
    ggplot2::geom_point(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        y = median
      ),
      color = "black",
      size = 0.75
    ) +
    ggplot2::scale_x_continuous(
      name = "Hour of the Day",
      breaks = seq(0, 23, 2),
      labels = sprintf("%02d:00", seq(0, 23, 2))
    ) +
    ggplot2::scale_fill_manual(
      name = "City",
      values = c("steelblue", "red")
    ) +
    ggplot2::scale_color_manual(
      name = "City",
      values = c("steelblue", "red")
    ) +
    ggplot2::labs(
      title = paste("LMP Distribution by Hour:", city1, "vs.", city2),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Locational Marginal Price ($/MWh)"
    ) +
    ggplot2::facet_wrap(~season, scales = "fixed", ncol = 2) + # Facet by season
    ggthemes::theme_solarized() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)
    )
  
  return(q)
}


#' Compare Congestion Prices between two cities visually
#' 
#' This script reads in two datasets containing prices for two different cities over a year (or other specified time period) and compares the distribution of LMPs by hour of the day.
#' 
#' @param df1 A dataframe of locational marginal prices for a year for one location. The default is San Francisco for 2023. Users can change this dataframe to analyze a different location or year by pulling from the `pulldata_node` function
#' @param df2 A dataframe of locational marginal prices for a year for another location. The default is Los Angeles for 2023. Users can change this dataframe to analyze a different location or year by pulling from the `pulldata_node` function
#' 
#' @return A ggplot object displaying the distribution of congestion prices by hour of the day for the two cities
#' 
#' @export
compare_congestion_visual = function(df1 = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR")), df2 = read.csv(system.file("extdata", "Los_Angeles_node_2023.csv", package = "CAISOWrapR"))){
  df1 <- df1 %>%
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S"),
      hour = as.numeric(substr(time_only, 1, 2))
    )
  
  df2 <- df2 %>%
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S"),
      hour = as.numeric(substr(time_only, 1, 2))
    )
  
  avg_price_per_time1 <- df1 %>%
    dplyr::filter(time_only == paste0(sprintf("%02d", hour), ":00:00")) %>% 
    dplyr::group_by(hour) %>%
    dplyr::summarise(
      lower_whisker = quantile(congestion, 0.1, na.rm = TRUE),
      lower_quartile = quantile(congestion, 0.25, na.rm = TRUE),
      median = median(congestion, na.rm = TRUE),
      upper_quartile = quantile(congestion, 0.75, na.rm = TRUE),
      upper_whisker = quantile(congestion, 0.9, na.rm = TRUE),
      city = dplyr::pull(df1, city)[1]
    )
  
  avg_price_per_time2 <- df2 %>%
    dplyr::filter(time_only == paste0(sprintf("%02d", hour), ":00:00")) %>% 
    dplyr::group_by(hour) %>%
    dplyr::summarise(
      lower_whisker = quantile(congestion, 0.1, na.rm = TRUE),
      lower_quartile = quantile(congestion, 0.25, na.rm = TRUE),
      median = median(congestion, na.rm = TRUE),
      upper_quartile = quantile(congestion, 0.75, na.rm = TRUE),
      upper_whisker = quantile(congestion, 0.9, na.rm = TRUE),
      city = dplyr::pull(df2, city)[1]
    )
  
  city1 <- unique(avg_price_per_time1$city)
  city2 <- unique(avg_price_per_time2$city)
  
  # Plot
  q <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = avg_price_per_time1,
      ggplot2::aes(
        xmin = hour - 0.35,
        xmax = hour - 0.005,
        ymin = lower_quartile,
        ymax = upper_quartile,
        fill = city
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = hour - 0.2,
        xend = hour - 0.2,
        y = lower_whisker,
        yend = lower_quartile,
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = hour - 0.2,
        xend = hour - 0.2,
        y = upper_quartile,
        yend = upper_whisker,
      ),
      color = "black"
    ) +
    ggplot2::geom_point(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = (hour - 0.35 + hour - 0.005) / 2,
        y = median,
      ),
      size = 0.75,
      color = "black"
    ) +
    ggplot2::geom_rect(
      data = avg_price_per_time2,
      ggplot2::aes(
        xmin = hour + 0.05,
        xmax = hour + 0.35,
        ymin = lower_quartile,
        ymax = upper_quartile,
        fill = city
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        xend = hour + 0.2,
        y = lower_whisker,
        yend = lower_quartile,
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        xend = hour + 0.2,
        y = upper_quartile,
        yend = upper_whisker,
      ),
      color = "black"
    ) +
    ggplot2::geom_point(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        y = median,
      ),
      color = "black",
      size = 0.75
    ) +
    ggplot2::scale_x_continuous(
      name = "Hour of the Day",
      breaks = seq(0, 23, 2),
      labels = sprintf("%02d:00", seq(0, 23, 2))
    ) +
    ggplot2::scale_fill_manual(
      name = "City",
      values = c("steelblue", "red")
    ) +
    ggplot2::scale_color_manual(
      name = "City",
      values = c("steelblue", "red")
    ) +
    ggplot2::labs(
      title = paste("Congestion Price Distribution by Hour:", city1, "vs.", city2),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Congestion Price ($/MWh)"
    ) +
    ggthemes::theme_solarized() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)
    )
  
  return(q)
}


#' Compare Congestion prices between two cities visually by Season
#' 
#' This script reads in two datasets containing congestion prices for two different cities over a year (or other specified time period) and compares the distribution of LMPs by hour of the day by season.
#' 
#' @param df1 A dataframe of locational marginal prices for a year for one location. The default is San Francisco for 2023. Users can change this dataframe to analyze a different location or year by pulling from the `pulldata_node` function.
#' @param df2 A dataframe of locational marginal prices for a year for another location. The default is Los Angeles for 2023. Users can change this dataframe to analyze a different location or year by pulling from the `pulldata_node` function.
#' 
#' @return A ggplot object displaying the distribution of congestion prices by hour of the day for the two cities by season.
#' 
#' @export
compare_congestion_visual_seasonal = function(df1 = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR")), df2 = read.csv(system.file("extdata", "Los_Angeles_node_2023.csv", package = "CAISOWrapR"))) {
  
  df1 <- df1 %>%
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S"),
      hour = as.numeric(substr(time_only, 1, 2)),
      month = as.numeric(format(interval_start_pst, "%m")),
      season = dplyr::case_when(
        month %in% c(12, 1, 2) ~ "Winter",
        month %in% c(3, 4, 5) ~ "Spring",
        month %in% c(6, 7, 8) ~ "Summer",
        month %in% c(9, 10, 11) ~ "Fall",
        TRUE ~ NA_character_
      )
    )
  
  df2 <- df2 %>%
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S"),
      hour = as.numeric(substr(time_only, 1, 2)),
      month = as.numeric(format(interval_start_pst, "%m")),
      season = dplyr::case_when(
        month %in% c(12, 1, 2) ~ "Winter",
        month %in% c(3, 4, 5) ~ "Spring",
        month %in% c(6, 7, 8) ~ "Summer",
        month %in% c(9, 10, 11) ~ "Fall",
        TRUE ~ NA_character_
      )
    )
  
  avg_price_per_time1 <- df1 %>%
    dplyr::filter(time_only == paste0(sprintf("%02d", hour), ":00:00")) %>% 
    dplyr::group_by(hour, season) %>%
    dplyr::summarise(
      lower_whisker = quantile(congestion, 0.1, na.rm = TRUE),
      lower_quartile = quantile(congestion, 0.25, na.rm = TRUE),
      median = median(congestion, na.rm = TRUE),
      upper_quartile = quantile(congestion, 0.75, na.rm = TRUE),
      upper_whisker = quantile(congestion, 0.9, na.rm = TRUE),
      city = dplyr::pull(df1, city)[1]
    )
  
  avg_price_per_time2 <- df2 %>%
    dplyr::filter(time_only == paste0(sprintf("%02d", hour), ":00:00")) %>% 
    dplyr::group_by(hour, season) %>%
    dplyr::summarise(
      lower_whisker = quantile(congestion, 0.1, na.rm = TRUE),
      lower_quartile = quantile(congestion, 0.25, na.rm = TRUE),
      median = median(congestion, na.rm = TRUE),
      upper_quartile = quantile(congestion, 0.75, na.rm = TRUE),
      upper_whisker = quantile(congestion, 0.9, na.rm = TRUE),
      city = dplyr::pull(df2, city)[1]
    )
  
  city1 <- unique(avg_price_per_time1$city)
  city2 <- unique(avg_price_per_time2$city)
  
  # Plot
  q <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = avg_price_per_time1,
      ggplot2::aes(
        xmin = hour - 0.35,
        xmax = hour - 0.005,
        ymin = lower_quartile,
        ymax = upper_quartile,
        fill = city
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = hour - 0.2,
        xend = hour - 0.2,
        y = lower_whisker,
        yend = lower_quartile,
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = hour - 0.2,
        xend = hour - 0.2,
        y = upper_quartile,
        yend = upper_whisker,
      ),
      color = "black"
    ) +
    ggplot2::geom_point(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = (hour - 0.35 + hour - 0.005) / 2,
        y = median,
      ),
      size = 0.75,
      color = "black"
    ) +
    ggplot2::geom_rect(
      data = avg_price_per_time2,
      ggplot2::aes(
        xmin = hour + 0.05,
        xmax = hour + 0.35,
        ymin = lower_quartile,
        ymax = upper_quartile,
        fill = city
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        xend = hour + 0.2,
        y = lower_whisker,
        yend = lower_quartile,
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        xend = hour + 0.2,
        y = upper_quartile,
        yend = upper_whisker,
      ),
      color = "black"
    ) +
    ggplot2::geom_point(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        y = median,
      ),
      color = "black",
      size = 0.75
    ) +
    ggplot2::scale_x_continuous(
      name = "Hour of the Day",
      breaks = seq(0, 23, 2),
      labels = sprintf("%02d:00", seq(0, 23, 2))
    ) +
    ggplot2::scale_fill_manual(
      name = "City",
      values = c("steelblue", "red")
    ) +
    ggplot2::scale_color_manual(
      name = "City",
      values = c("steelblue", "red")
    ) +
    ggplot2::labs(
      title = paste("Congestion Price Distribution by Hour:", city1, "vs.", city2),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Congestion Price ($/MWh)"
    ) +
    ggplot2::facet_wrap(~season, scales = "fixed", ncol = 2) + # Facet by season
    ggthemes::theme_solarized() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)
    )
  
  return(q)
  
}



#' Compare Loss Pricing Adjustments between two cities visually
#' 
#' This script reads in two datasets containing prices for two different cities over a year (or other specified time period) and compares the distribution of LMPs by hour of the day.
#' 
#' @param df1 A dataframe of locational marginal prices for a year for one location. The default is San Francisco for 2023. Users can change this dataframe to analyze a different location or year by pulling from the `pulldata_node` function
#' @param df2 A dataframe of locational marginal prices for a year for another location. The defualt is Los Angeles for 2023. Users can change this dataframe to analyze a different location or year by pulling from the `pulldata_node` function
#' 
#' @return A ggplot object displaying the distribution of loss pricing adjustments by hour of the day for the two cities
#' 
#' @export
compare_losses_visual = function(df1 = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR")), df2 = read.csv(system.file("extdata", "Los_Angeles_node_2023.csv", package = "CAISOWrapR"))){
  df1 <- df1 %>%
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S"),
      hour = as.numeric(substr(time_only, 1, 2))
    )
  
  df2 <- df2 %>%
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S"),
      hour = as.numeric(substr(time_only, 1, 2))
    )
  
  avg_price_per_time1 <- df1 %>%
    dplyr::filter(time_only == paste0(sprintf("%02d", hour), ":00:00")) %>% 
    dplyr::group_by(hour) %>%
    dplyr::summarise(
      lower_whisker = quantile(loss, 0.1, na.rm = TRUE),
      lower_quartile = quantile(loss, 0.25, na.rm = TRUE),
      median = median(loss, na.rm = TRUE),
      upper_quartile = quantile(loss, 0.75, na.rm = TRUE),
      upper_whisker = quantile(loss, 0.9, na.rm = TRUE),
      city = dplyr::pull(df1, city)[1]
    )
  
  avg_price_per_time2 <- df2 %>%
    dplyr::filter(time_only == paste0(sprintf("%02d", hour), ":00:00")) %>% 
    dplyr::group_by(hour) %>%
    dplyr::summarise(
      lower_whisker = quantile(loss, 0.1, na.rm = TRUE),
      lower_quartile = quantile(loss, 0.25, na.rm = TRUE),
      median = median(loss, na.rm = TRUE),
      upper_quartile = quantile(loss, 0.75, na.rm = TRUE),
      upper_whisker = quantile(loss, 0.9, na.rm = TRUE),
      city = dplyr::pull(df2, city)[1]
    )
  
  city1 <- unique(avg_price_per_time1$city)
  city2 <- unique(avg_price_per_time2$city)
  
  # Plot
  q <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = avg_price_per_time1,
      ggplot2::aes(
        xmin = hour - 0.35,
        xmax = hour - 0.005,
        ymin = lower_quartile,
        ymax = upper_quartile,
        fill = city
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = hour - 0.2,
        xend = hour - 0.2,
        y = lower_whisker,
        yend = lower_quartile,
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = hour - 0.2,
        xend = hour - 0.2,
        y = upper_quartile,
        yend = upper_whisker,
      ),
      color = "black"
    ) +
    ggplot2::geom_point(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = (hour - 0.35 + hour - 0.005) / 2,
        y = median,
      ),
      size = 0.75,
      color = "black"
    ) +
    ggplot2::geom_rect(
      data = avg_price_per_time2,
      ggplot2::aes(
        xmin = hour + 0.05,
        xmax = hour + 0.35,
        ymin = lower_quartile,
        ymax = upper_quartile,
        fill = city
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        xend = hour + 0.2,
        y = lower_whisker,
        yend = lower_quartile,
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        xend = hour + 0.2,
        y = upper_quartile,
        yend = upper_whisker,
      ),
      color = "black"
    ) +
    ggplot2::geom_point(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        y = median,
      ),
      color = "black",
      size = 0.75
    ) +
    ggplot2::scale_x_continuous(
      name = "Hour of the Day",
      breaks = seq(0, 23, 2),
      labels = sprintf("%02d:00", seq(0, 23, 2))
    ) +
    ggplot2::scale_fill_manual(
      name = "City",
      values = c("steelblue", "red")
    ) +
    ggplot2::scale_color_manual(
      name = "City",
      values = c("steelblue", "red")
    ) +
    ggplot2::labs(
      title = paste("Loss Pricing Adjustment Distribution by Hour:", city1, "vs.", city2),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Loss Pricing Adjustment ($/MWh)"
    ) +
    ggthemes::theme_solarized() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)
    )
  
  return(q)
}


#' Compare loss pricing adjustments between two cities visually by Season
#' 
#' This script reads in two datasets containing congestion prices for two different cities over a year (or other specified time period) and compares the distribution of LMPs by hour of the day by season.
#' 
#' @param df1 A dataframe of locational marginal prices for a year for one location. The default is San Francisco for 2023. Users can change this dataframe to analyze a different location or year by pulling from the `pulldata_node` function.
#' @param df2 A dataframe of locational marginal prices for a year for another location. The default is Los Angeles for 2023. Users can change this dataframe to analyze a different location or year by pulling from the `pulldata_node` function.
#' 
#' @return A ggplot object displaying the distribution of loss pricing adjustments by hour of the day for the two cities by season.
#' 
#' @export
compare_losses_visual_seasonal = function(df1 = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR")), df2 = read.csv(system.file("extdata", "Los_Angeles_node_2023.csv", package = "CAISOWrapR"))){
  
  df1 <- df1 %>%
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S"),
      hour = as.numeric(substr(time_only, 1, 2)),
      month = as.numeric(format(interval_start_pst, "%m")),
      season = dplyr::case_when(
        month %in% c(12, 1, 2) ~ "Winter",
        month %in% c(3, 4, 5) ~ "Spring",
        month %in% c(6, 7, 8) ~ "Summer",
        month %in% c(9, 10, 11) ~ "Fall",
        TRUE ~ NA_character_
      )
    )
  
  df2 <- df2 %>%
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S"),
      hour = as.numeric(substr(time_only, 1, 2)),
      month = as.numeric(format(interval_start_pst, "%m")),
      season = dplyr::case_when(
        month %in% c(12, 1, 2) ~ "Winter",
        month %in% c(3, 4, 5) ~ "Spring",
        month %in% c(6, 7, 8) ~ "Summer",
        month %in% c(9, 10, 11) ~ "Fall",
        TRUE ~ NA_character_
      )
    )
  
  avg_price_per_time1 <- df1 %>%
    dplyr::filter(time_only == paste0(sprintf("%02d", hour), ":00:00")) %>% 
    dplyr::group_by(hour, season) %>%
    dplyr::summarise(
      lower_whisker = quantile(loss, 0.1, na.rm = TRUE),
      lower_quartile = quantile(loss, 0.25, na.rm = TRUE),
      median = median(loss, na.rm = TRUE),
      upper_quartile = quantile(loss, 0.75, na.rm = TRUE),
      upper_whisker = quantile(loss, 0.9, na.rm = TRUE),
      city = dplyr::pull(df1, city)[1]
    )
  
  avg_price_per_time2 <- df2 %>%
    dplyr::filter(time_only == paste0(sprintf("%02d", hour), ":00:00")) %>% 
    dplyr::group_by(hour, season) %>%
    dplyr::summarise(
      lower_whisker = quantile(loss, 0.1, na.rm = TRUE),
      lower_quartile = quantile(loss, 0.25, na.rm = TRUE),
      median = median(loss, na.rm = TRUE),
      upper_quartile = quantile(loss, 0.75, na.rm = TRUE),
      upper_whisker = quantile(loss, 0.9, na.rm = TRUE),
      city = dplyr::pull(df2, city)[1]
    )
  
  city1 <- unique(avg_price_per_time1$city)
  city2 <- unique(avg_price_per_time2$city)
  
  # Plot
  q <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = avg_price_per_time1,
      ggplot2::aes(
        xmin = hour - 0.35,
        xmax = hour - 0.005,
        ymin = lower_quartile,
        ymax = upper_quartile,
        fill = city
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = hour - 0.2,
        xend = hour - 0.2,
        y = lower_whisker,
        yend = lower_quartile,
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = hour - 0.2,
        xend = hour - 0.2,
        y = upper_quartile,
        yend = upper_whisker,
      ),
      color = "black"
    ) +
    ggplot2::geom_point(
      data = avg_price_per_time1,
      ggplot2::aes(
        x = (hour - 0.35 + hour - 0.005) / 2,
        y = median,
      ),
      size = 0.75,
      color = "black"
    ) +
    ggplot2::geom_rect(
      data = avg_price_per_time2,
      ggplot2::aes(
        xmin = hour + 0.05,
        xmax = hour + 0.35,
        ymin = lower_quartile,
        ymax = upper_quartile,
        fill = city
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        xend = hour + 0.2,
        y = lower_whisker,
        yend = lower_quartile,
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        xend = hour + 0.2,
        y = upper_quartile,
        yend = upper_whisker,
      ),
      color = "black"
    ) +
    ggplot2::geom_point(
      data = avg_price_per_time2,
      ggplot2::aes(
        x = hour + 0.2,
        y = median,
      ),
      color = "black",
      size = 0.75
    ) +
    ggplot2::scale_x_continuous(
      name = "Hour of the Day",
      breaks = seq(0, 23, 2),
      labels = sprintf("%02d:00", seq(0, 23, 2))
    ) +
    ggplot2::scale_fill_manual(
      name = "City",
      values = c("steelblue", "red")
    ) +
    ggplot2::scale_color_manual(
      name = "City",
      values = c("steelblue", "red")
    ) +
    ggplot2::labs(
      title = paste("Loss Pricing Adjustment Distribution by Hour:", city1, "vs.", city2),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Loss Pricing Adjustment ($/MWh)"
    ) +
    ggplot2::facet_wrap(~season, scales = "fixed", ncol = 2) + # Facet by season
    ggthemes::theme_solarized() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)
    )
  
  return(q)
  
}

