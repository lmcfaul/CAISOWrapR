# This script has all of the functions that analyze a single node of data (one location with all 15 minute intervals for a year) and combines the results into a pdf to export. The functions `visualize_node` and `visualize_node_seasonal` are called with an inputting dataframe of a node of data. The user has to call the `pulldata_node` function to get the data for the node they want to analyze. The user can also just call the individual analysis functions if they want to see the results separately.


#' Analyze and Visualize Data for a Single Node
#' 
#' This function analyzes data from a single node, representing one location with locational marginal prices (LMPs) over a year (at 15-minute intervals). It calls multiple visualization functions to generate a series of plots, which are then compiled into a PDF report for easy export. The analysis covers various aspects of the node's data, including LMP distributions, congestion, loss pricing adjustments, and demand.
#' 
#' The user should first call the `pulldata_node` function to load the required data for the specific node. Then, they can call this function to generate a comprehensive report with visualizations that focus on different aspects of the node's data. Alternatively, the user can call the individual analysis functions separately to examine specific aspects of the data without generating the full report.
#' 
#' The following functions are called within this process:
#' \itemize{
#'   \item `visualize_node`: Creates plots visualizing the distribution of LMPs for the node.
#'   \item `visualize_node_seasonal`: Generates seasonal plots showing trends in LMPs over the course of the year.
#'   \item `visualize_node_congestion`: Creates visualizations for congestion at the node.
#'   \item `visualize_node_seasonal_congestion`: Generates seasonal plots for congestion analysis.
#'   \item `visualize_node_losses`: Produces plots for loss pricing adjustments at the node.
#'   \item `visualize_node_seasonal_losses`: Generates seasonal plots for loss pricing adjustments.
#'   \item `visualize_node_demand`: Creates plots analyzing demand at the node over the year.
#'   \item `visualize_node_demand_without_high_demand_hours`: Visualizes demand at the node, excluding high-demand hours.
#'   \item `visualize_node_demand_congestion`: Visualizes the relationship between demand and congestion.
#'   \item `visualize_node_demand_without_high_demand_hours_congestion`: Visualizes the relationship between demand and congestion, excluding high-demand hours.
#' }
#' 
#' @param node_df A dataframe of locational marginal prices for a year for one node. The default for this is San Francisco 2023, but a user can input a different dataframe by running the `pulldata_node` function.
#' @param output_pdf_path The path to save the pdf file. There is a default path of "single_instance_data.pdf" but the user can edit this path to save the file in a different location.
#' 
#' @return A pdf file with the two plots
#'
#' @export
analyze_single_node_data <- function(node_df = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR")), output_pdf_path = "single_node_data.pdf") {
  # Visualizations for LMP
  result_one <- visualize_node(node_df)
  result_two <- visualize_node_seasonal(node_df)
  
  # Visualizations for congestion
  result_three <- visualize_node_congestion(node_df)
  result_four <- visualize_node_seasonal_congestion(node_df)
  
  # Visualizations for Loss Pricing Adjustments
  result_five <- visualize_node_losses(node_df)
  result_six <- visualize_node_seasonal_losses(node_df)
  
  # Visualizations for Demand
  result_seven <- visualize_node_demand(node_df)
  result_eight <- visualize_node_demand_without_high_demand_hours(node_df)
  result_nine <- visualize_node_demand_congestion(node_df)
  result_ten <- visualize_node_demand_without_high_demand_hours_congestion(node_df)
  
  # Extract the year and city for the title
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(lubridate::ymd_hms(interval_time), "%Y")
  
  title <- grid::textGrob(
    paste("LMP Distributions for", node_df$city[1], "in", year_of_data),
    gp = grid::gpar(fontsize = 16, fontface = "bold"),
    just = "center"
  )
  
  # Add margins to each plot
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
  
  plot_with_margins_7 <- gtable_add_padding1(
    ggplot2::ggplotGrob(result_seven),
    padding = grid::unit(c(1, 1, 1, 1), "cm")
  )
  
  plot_with_margins_8 <- gtable_add_padding1(
    ggplot2::ggplotGrob(result_eight),
    padding = grid::unit(c(1, 1, 1, 1), "cm")
  )
  
  plot_with_margins_9 <- gtable_add_padding1(
    ggplot2::ggplotGrob(result_nine),
    padding = grid::unit(c(1, 1, 1, 1), "cm")
  )
  
  plot_with_margins_10 <- gtable_add_padding1(
    ggplot2::ggplotGrob(result_ten),
    padding = grid::unit(c(1, 1, 1, 1), "cm")
  )
  
  # Create the PDF file
  pdf(output_pdf_path, width = 8.5, height = 11)
  
  # First page: LMP plots
  gridExtra::grid.arrange(
    title,
    plot_with_margins_1,
    plot_with_margins_2,
    nrow = 3,
    heights = c(0.1, 0.45, 0.45)
  )
  
  # Second page: Congestion plots
  gridExtra::grid.arrange(
    grid::textGrob(
      paste("Congestion for", node_df$city[1], "in", year_of_data),
      gp = grid::gpar(fontsize = 16, fontface = "bold"),
      just = "center"
    ),
    plot_with_margins_3,
    plot_with_margins_4,
    nrow = 3,
    heights = c(0.1, 0.45, 0.45)
  )
  
  # Third page: Loss Pricing Adjustments plots
  gridExtra::grid.arrange(
    grid::textGrob(
      paste("Loss Pricing Adjustments for", node_df$city[1], "in", year_of_data),
      gp = grid::gpar(fontsize = 16, fontface = "bold"),
      just = "center"
    ),
    plot_with_margins_5,
    plot_with_margins_6,
    nrow = 3,
    heights = c(0.1, 0.45, 0.45)
  )
  
  # Fourth page: LMP and Demand plots
  gridExtra::grid.arrange(
    grid::textGrob(
      paste("LMP and Demand Relationship for", node_df$city[1], "in", year_of_data),
      gp = grid::gpar(fontsize = 16, fontface = "bold"),
      just = "center"
    ),
    plot_with_margins_7,
    plot_with_margins_8,
    nrow = 3,
    heights = c(0.1, 0.45, 0.45)
  )
  
  # Fifth page: Congestion price and Demand plots
  gridExtra::grid.arrange(
    grid::textGrob(
      paste("Congestion Price and Demand Relationship for", node_df$city[1], "in", year_of_data),
      gp = grid::gpar(fontsize = 16, fontface = "bold"),
      just = "center"
    ),
    plot_with_margins_9,
    plot_with_margins_10,
    nrow = 3,
    heights = c(0.1, 0.45, 0.45)
  )
  # Close the PDF file
  dev.off()
  
  message("Results saved to: ", output_pdf_path)
}


#' Visualize Node Data
#' 
#' This function takes in a dataframe of a year's worth of 15-minute data for a node
#' and visualizes the data in a box and whisker plot by interval to see how the price distribution ranges
#' over a day.
#' 
#' @param node_df A dataframe of locational marginal prices for a year for one node. The default for this is San Francisco 2023, but a user can input a different dataframe by running the `pulldata_node` function.
#' 
#' @return A `ggplot2` object with a box and whisker plot.
#'
#' @export
visualize_node <- function(node_df = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR"))) {
  
  node_df <- node_df |>
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S")
    )
  
  avg_price_per_time <- node_df |>
    dplyr::group_by(time_only) |>
    dplyr::summarise(
      lower_whisker = quantile(lmp, 0.1, na.rm = TRUE),
      lower_quartile = quantile(lmp, 0.25, na.rm = TRUE),
      median = median(lmp, na.rm = TRUE),
      upper_quartile = quantile(lmp, 0.75, na.rm = TRUE),
      upper_whisker = quantile(lmp, 0.9, na.rm = TRUE)
    )
  
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(lubridate::ymd_hms(interval_time), "%Y")
  
  p <- ggplot2::ggplot(avg_price_per_time, ggplot2::aes(x = time_only)) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = as.numeric(factor(time_only)) - 0.4,
        xmax = as.numeric(factor(time_only)) + 0.4,
        ymin = lower_quartile,
        ymax = upper_quartile,
      ),
      fill = "steelblue",
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(time_only)),
        xend = as.numeric(factor(time_only)),
        y = lower_whisker,
        yend = lower_quartile
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(time_only)),
        xend = as.numeric(factor(time_only)),
        y = upper_quartile,
        yend = upper_whisker
      ),
      color = "black"
    ) +
    ggplot2::geom_point(ggplot2::aes(
      x = as.numeric(factor(time_only)),
      y = median
    ), color = "black", size = .75) +
    ggplot2::scale_x_discrete(
      name = "Hour of the Day",
      breaks = avg_price_per_time$time_only[avg_price_per_time$time_only %in% sprintf("%02d:00:00", seq(0, 22, 2))],
      labels = sprintf("%02d:00", seq(0, 22, 2))
    ) +
    ggplot2::labs(
      title = paste("LMP Distribution by 15-minute Interval for", node_df$city[1], "in", year_of_data), 
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Locational Marginal Price ($/MWh)",
      fill = "Median LMP"
    ) +
    ggthemes::theme_solarized() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
  
  return(p)
  
}


#' Visualize distribution of LMPs in one location by season
#' 
#' This script reads in a dataframe of LMP data for a city over a year and visualizes the distribution of LMPs by hour for each season.
#' 
#' @param node_df A dataframe of locational marginal prices for a year for one node. The default for this is San Francisco 2023, but a user can input a different dataframe by running the `pulldata_node` function.
#' 
#' @return A `ggplot2` object displaying the distribution of LMPs by hour for the location over a day.
#' 
#' @export
visualize_node_seasonal <- function(node_df = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR"))) {
  
  node_df <- node_df |>
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
    ) |>
    dplyr::filter(!is.na(season))
  
  avg_price_per_time <- node_df |>
    dplyr::group_by(hour, season) |>
    dplyr::summarise(
      lower_whisker = quantile(lmp, 0.1, na.rm = TRUE),
      lower_quartile = quantile(lmp, 0.25, na.rm = TRUE),
      median = median(lmp, na.rm = TRUE),
      upper_quartile = quantile(lmp, 0.75, na.rm = TRUE),
      upper_whisker = quantile(lmp, 0.9, na.rm = TRUE),
      .groups = "drop"
    )
  
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(lubridate::ymd_hms(interval_time), "%Y")
  
  x_breaks <- seq(0, 23, by = 2)
  x_labels <- sprintf("%02d:00", x_breaks)
  
  p <- ggplot2::ggplot(avg_price_per_time, ggplot2::aes(x = hour)) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = as.numeric(factor(hour)) - 0.4,
        xmax = as.numeric(factor(hour)) + 0.4,
        ymin = lower_quartile,
        ymax = upper_quartile
      ),
      fill = "steelblue",
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(hour)),
        xend = as.numeric(factor(hour)),
        y = lower_whisker,
        yend = lower_quartile
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(hour)),
        xend = as.numeric(factor(hour)),
        y = upper_quartile,
        yend = upper_whisker
      ),
      color = "black"
    ) +
    ggplot2::geom_point(ggplot2::aes(
      x = as.numeric(factor(hour)),
      y = median
    ), color = "black", size = 0.75) +
    ggplot2::scale_x_continuous(
      name = "Hour of the Day",
      breaks = x_breaks,
      labels = x_labels
    ) +
    ggplot2::labs(
      title = paste("Seasonal LMP Distribution by Hour for", node_df$city[1], "in", year_of_data),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Locational Marginal Price ($/MWh)",
      fill = "Median LMP"
    ) +
    ggplot2::facet_wrap(~season, scales = "fixed", ncol = 2) + 
    ggthemes::theme_solarized() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)
    )
  
  return(p)
}


#' Visualize Node Data: Congestion
#' 
#' This function takes in a dataframe of a year's worth of 15-minute data for a node
#' and visualizes the data in a box and whisker plot by interval to see how the price distribution ranges
#' over a day.
#' 
#' @param node_df A dataframe of locational marginal prices for a year for one node. The default for this is San Francisco 2023, but a user can input a different dataframe by running the `pulldata_node` function.
#' 
#' @return A `ggplot2` object displaying the congestion price distribution by 15-minute interval.
#' 
#' @export
visualize_node_congestion = function(node_df = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR"))) {
  
  node_df <- node_df |>
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S")
    )
  
  avg_price_per_time <- node_df |>
    dplyr::group_by(time_only) |>
    dplyr::summarise(
      lower_whisker = quantile(congestion, 0.1, na.rm = TRUE),
      lower_quartile = quantile(congestion, 0.25, na.rm = TRUE),
      median = median(congestion, na.rm = TRUE),
      upper_quartile = quantile(congestion, 0.75, na.rm = TRUE),
      upper_whisker = quantile(congestion, 0.9, na.rm = TRUE),
      .groups = "drop"
    )
  
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(lubridate::ymd_hms(interval_time), "%Y")
  
  p <- ggplot2::ggplot(avg_price_per_time, ggplot2::aes(x = time_only)) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = as.numeric(factor(time_only)) - 0.4,
        xmax = as.numeric(factor(time_only)) + 0.4,
        ymin = lower_quartile,
        ymax = upper_quartile,
      ),
      fill = "steelblue",
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(time_only)),
        xend = as.numeric(factor(time_only)),
        y = lower_whisker,
        yend = lower_quartile
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(time_only)),
        xend = as.numeric(factor(time_only)),
        y = upper_quartile,
        yend = upper_whisker
      ),
      color = "black"
    ) +
    ggplot2::geom_point(ggplot2::aes(
      x = as.numeric(factor(time_only)),
      y = median
    ), color = "black", size = 0.75) +
    ggplot2::scale_x_discrete(
      name = "Hour of the Day",
      breaks = avg_price_per_time$time_only[avg_price_per_time$time_only %in% sprintf("%02d:00:00", seq(0, 22, 2))],
      labels = sprintf("%02d:00", seq(0, 22, 2))
    ) +
    ggplot2::labs(
      title = paste("Congestion Price Distribution by 15-minute Interval for", node_df$city[1], "in", year_of_data), 
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Congestion Price ($/MWh)",
      fill = "Median Congestion Price"
    ) +
    ggthemes::theme_solarized() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
  
  return(p)
}



#' Visualize distribution of Congestion prices in one location by season
#' 
#' This script reads in a dataframe of LMP data for a city over a year and visualizes the distribution of LMPs by hour for each season.
#' 
#' @param node_df A dataframe of locational marginal prices for a year for one node. The default for this is San Francisco 2023, but a user can input a different dataframe by running the `pulldata_node` function.
#' 
#' @return A `ggplot2` object displaying the distribution of LMPs by hour for the location over a day, split by season.
#' 
#' @export
visualize_node_seasonal_congestion = function(node_df = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR"))) {
  
  node_df <- node_df |>
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
    ) |>
    dplyr::filter(!is.na(season))
  
  avg_price_per_time <- node_df |>
    dplyr::group_by(hour, season) |>
    dplyr::summarise(
      lower_whisker = quantile(congestion, 0.1, na.rm = TRUE),
      lower_quartile = quantile(congestion, 0.25, na.rm = TRUE),
      median = median(congestion, na.rm = TRUE),
      upper_quartile = quantile(congestion, 0.75, na.rm = TRUE),
      upper_whisker = quantile(congestion, 0.9, na.rm = TRUE),
      .groups = "drop"
    )
  
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(lubridate::ymd_hms(interval_time), "%Y")
  
  x_breaks <- seq(0, 23, by = 2)
  x_labels <- sprintf("%02d:00", x_breaks)
  
  p <- ggplot2::ggplot(avg_price_per_time, ggplot2::aes(x = hour)) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = as.numeric(factor(hour)) - 0.4,
        xmax = as.numeric(factor(hour)) + 0.4,
        ymin = lower_quartile,
        ymax = upper_quartile
      ),
      fill = "steelblue",
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(hour)),
        xend = as.numeric(factor(hour)),
        y = lower_whisker,
        yend = lower_quartile
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(hour)),
        xend = as.numeric(factor(hour)),
        y = upper_quartile,
        yend = upper_whisker
      ),
      color = "black"
    ) +
    ggplot2::geom_point(ggplot2::aes(
      x = as.numeric(factor(hour)),
      y = median
    ), color = "black", size = 0.75) +
    ggplot2::scale_x_continuous(
      name = "Hour of the Day",
      breaks = x_breaks,
      labels = x_labels
    ) +
    ggplot2::labs(
      title = paste("Seasonal Congestion Price Distribution by Hour for", node_df$city[1], "in", year_of_data),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Congestion Price ($/MWh)",
      fill = "Median Congestion Price"
    ) +
    ggplot2::facet_wrap(~season, scales = "fixed", ncol = 2) + 
    ggthemes::theme_solarized() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)
    )
  
  return(p)
}


#' Visualize Node Data: Loss Pricing Adjustment
#' 
#' This function takes in a dataframe of a year's worth of 15-minute data for a node
#' and visualizes the data in a box and whisker plot by interval to see how the price distribution ranges
#' over a day.
#' 
#' @param node_df A dataframe of locational marginal prices for a year for one node. The default for this is San Francisco 2023, but a user can input a different dataframe by running the `pulldata_node` function.
#' 
#' @return A `ggplot2` object displaying a box and whisker plot for the loss pricing adjustments by time interval.
#' 
#' @export
visualize_node_losses = function(node_df = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR"))) {
  
  node_df <- node_df |>
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S")
    )
  
  avg_price_per_time <- node_df |>
    dplyr::group_by(time_only) |>
    dplyr::summarise(
      lower_whisker = quantile(loss, 0.1, na.rm = TRUE),
      lower_quartile = quantile(loss, 0.25, na.rm = TRUE),
      median = median(loss, na.rm = TRUE),
      upper_quartile = quantile(loss, 0.75, na.rm = TRUE),
      upper_whisker = quantile(loss, 0.9, na.rm = TRUE)
    )
  
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(lubridate::ymd_hms(interval_time), "%Y")
  
  p <- ggplot2::ggplot(avg_price_per_time, ggplot2::aes(x = time_only)) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = as.numeric(factor(time_only)) - 0.4,
        xmax = as.numeric(factor(time_only)) + 0.4,
        ymin = lower_quartile,
        ymax = upper_quartile
      ),
      fill = "steelblue",
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(time_only)),
        xend = as.numeric(factor(time_only)),
        y = lower_whisker,
        yend = lower_quartile
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(time_only)),
        xend = as.numeric(factor(time_only)),
        y = upper_quartile,
        yend = upper_whisker
      ),
      color = "black"
    ) +
    ggplot2::geom_point(ggplot2::aes(
      x = as.numeric(factor(time_only)),
      y = median
    ), color = "black", size = .75) +
    ggplot2::scale_x_discrete(
      name = "Hour of the Day",
      breaks = avg_price_per_time$time_only[avg_price_per_time$time_only %in% sprintf("%02d:00:00", seq(0, 22, 2))],
      labels = sprintf("%02d:00", seq(0, 22, 2))
    ) +
    ggplot2::labs(
      title = paste("Loss Pricing Adjustment Distribution by 15-minute Interval for", node_df$city[1], "in", year_of_data), 
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Loss Pricing Adjustment ($/MWh)",
      fill = "Median Loss Pricing Adjustment"
    ) +
    ggthemes::theme_solarized() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
  
  return(p)
}




#' Visualize Distribution of Loss Pricing Adjustments by Season
#' 
#' This function reads in a dataframe of LMP data for a city over a year and visualizes
#' the distribution of LMPs by hour for each season (Winter, Spring, Summer, Fall).
#' 
#' @param node_df A dataframe of locational marginal prices for a year for one node. The default for this is San Francisco 2023, but a user can input a different dataframe by running the `pulldata_node` function.
#' 
#' @return A `ggplot2` object displaying the distribution of LMPs by hour for the location over the day, separated by season.
#' 
#' @export
visualize_node_seasonal_losses = function(node_df = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR"))) {
  
  node_df <- node_df |>
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
    ) |>
    dplyr::filter(!is.na(season))
  
  avg_price_per_time <- node_df |>
    dplyr::group_by(hour, season) |>
    dplyr::summarise(
      lower_whisker = quantile(loss, 0.1, na.rm = TRUE),
      lower_quartile = quantile(loss, 0.25, na.rm = TRUE),
      median = median(loss, na.rm = TRUE),
      upper_quartile = quantile(loss, 0.75, na.rm = TRUE),
      upper_whisker = quantile(loss, 0.9, na.rm = TRUE),
      .groups = "drop"
    )
  
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(lubridate::ymd_hms(interval_time), "%Y")
  
  x_breaks <- seq(0, 23, by = 2)
  x_labels <- sprintf("%02d:00", x_breaks)
  
  p <- ggplot2::ggplot(avg_price_per_time, ggplot2::aes(x = hour)) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = as.numeric(factor(hour)) - 0.4,
        xmax = as.numeric(factor(hour)) + 0.4,
        ymin = lower_quartile,
        ymax = upper_quartile
      ),
      fill = "steelblue",
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(hour)),
        xend = as.numeric(factor(hour)),
        y = lower_whisker,
        yend = lower_quartile
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(hour)),
        xend = as.numeric(factor(hour)),
        y = upper_quartile,
        yend = upper_whisker
      ),
      color = "black"
    ) +
    ggplot2::geom_point(ggplot2::aes(
      x = as.numeric(factor(hour)),
      y = median
    ), color = "black", size = 0.75) +
    ggplot2::scale_x_continuous(
      name = "Hour of the Day",
      breaks = x_breaks,
      labels = x_labels
    ) +
    ggplot2::labs(
      title = paste("Seasonal Loss Pricing Adjustment Distribution by Hour for", node_df$city[1], "in", year_of_data),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Loss Pricing Adjustment ($/MWh)",
      fill = "Median Loss Pricing Adjustment"
    ) +
    ggplot2::facet_wrap(~season, scales = "fixed", ncol = 2) + 
    ggthemes::theme_solarized() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)
    )
  
  return(p)
}




#' Visualize Node Data: Demand
#' 
#' This function takes in a dataframe of a year's worth of 15-minute data for a node and visualizes the data with a box and whisker plot, using buckets of load on the x-axis and LMP on the y-axis.
#' 
#' @param node_df A dataframe of locational marginal prices for a year for one node. The default for this is San Francisco 2023, but a user can input a different dataframe by running the `pulldata_node` function.
#' 
#' @return A `ggplot2` object displaying the distribution of LMPs by demand bucket.
#' 
#' @export
visualize_node_demand <- function(node_df = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR"))) {
  
  node_df = merge_demand(node_df)
  
  # Create load buckets ranging from 12,000 to 52,000 with 2,000 intervals
  node_df <- node_df |>
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      load_bucket = cut(load, 
                        breaks = seq(12000, 52000, by = 1000), 
                        include.lowest = TRUE,
                        labels = paste0(seq(12, 51, by = 1), "-", seq(14, 52, by = 1)))
    )
  
  # Summarize LMP data by load bucket
  avg_price_per_bucket <- node_df |>
    dplyr::group_by(load_bucket) |>
    dplyr::summarise(
      lower_whisker = quantile(lmp, 0.1, na.rm = TRUE),
      lower_quartile = quantile(lmp, 0.25, na.rm = TRUE),
      median = median(lmp, na.rm = TRUE),
      upper_quartile = quantile(lmp, 0.75, na.rm = TRUE),
      upper_whisker = quantile(lmp, 0.9, na.rm = TRUE)
    )
  
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(lubridate::ymd_hms(interval_time), "%Y")
  
  # Plot the results
  p <- ggplot2::ggplot(avg_price_per_bucket, ggplot2::aes(x = load_bucket)) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = as.numeric(factor(load_bucket)) - 0.4,
        xmax = as.numeric(factor(load_bucket)) + 0.4,
        ymin = lower_quartile,
        ymax = upper_quartile
      ),
      fill = "steelblue",
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(load_bucket)),
        xend = as.numeric(factor(load_bucket)),
        y = lower_whisker,
        yend = lower_quartile
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(load_bucket)),
        xend = as.numeric(factor(load_bucket)),
        y = upper_quartile,
        yend = upper_whisker
      ),
      color = "black"
    ) +
    ggplot2::geom_point(ggplot2::aes(
      x = as.numeric(factor(load_bucket)),
      y = median
    ), color = "black", size = .75) +
    ggplot2::scale_x_discrete(
      name = "Load Buckets (Thousand MWs)",
      labels = levels(avg_price_per_bucket$load_bucket)
    ) +
    ggplot2::labs(
      title = paste("LMP Distribution by Load Bucket for", node_df$city[1], "in", year_of_data),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Locational Marginal Price ($/MWh)",
      fill = "Median LMP"
    ) +
    ggthemes::theme_solarized() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(p)
}

#' Visualize Node Data Without High Demand Days
#' 
#' This function takes in a tibble of a year's worth of 15-minute data for a node and visualizes the data with a box and whisker plot of LMP grouped by load buckets, excluding high-demand days.
#' 
#' @param node_df A dataframe of locational marginal prices for a year for one node. The default for this is San Francisco 2023, but a user can input a different dataframe by running the `pulldata_node` function.
#' 
#' @return A `ggplot2` object displaying the distribution of LMPs by demand bucket (up to 40,000).
#' 
#' @export
visualize_node_demand_without_high_demand_hours <- function(node_df = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR"))){
  
  node_df = merge_demand(node_df)
  
  # Filter out high-demand loads
  node_df <- node_df |> dplyr::filter(load <= 40000)
  
  # Create load buckets ranging from 12,000 to 40,000 with 1,000 intervals
  node_df <- node_df |>
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      load_bucket = cut(load, 
                        breaks = seq(12000, 40000, by = 1000), 
                        include.lowest = TRUE,
                        labels = paste0(seq(12, 39, by = 1), "-", seq(13, 40, by = 1)))
    )
  
  # Summarize LMP data by load bucket
  avg_price_per_bucket <- node_df |>
    dplyr::group_by(load_bucket) |>
    dplyr::summarise(
      lower_whisker = quantile(lmp, 0.1, na.rm = TRUE),
      lower_quartile = quantile(lmp, 0.25, na.rm = TRUE),
      median = median(lmp, na.rm = TRUE),
      upper_quartile = quantile(lmp, 0.75, na.rm = TRUE),
      upper_whisker = quantile(lmp, 0.9, na.rm = TRUE)
    )
  
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(lubridate::ymd_hms(interval_time), "%Y")
  
  # Plot the results
  p <- ggplot2::ggplot(avg_price_per_bucket, ggplot2::aes(x = load_bucket)) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = as.numeric(factor(load_bucket)) - 0.4,
        xmax = as.numeric(factor(load_bucket)) + 0.4,
        ymin = lower_quartile,
        ymax = upper_quartile
      ),
      fill = "steelblue",
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(load_bucket)),
        xend = as.numeric(factor(load_bucket)),
        y = lower_whisker,
        yend = lower_quartile
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(load_bucket)),
        xend = as.numeric(factor(load_bucket)),
        y = upper_quartile,
        yend = upper_whisker
      ),
      color = "black"
    ) +
    ggplot2::geom_point(ggplot2::aes(
      x = as.numeric(factor(load_bucket)),
      y = median
    ), color = "black", size = .75) +
    ggplot2::scale_x_discrete(
      name = "Load Buckets (Thousand MWs)",
      labels = levels(avg_price_per_bucket$load_bucket)
    ) +
    ggplot2::labs(
      title = paste("LMP Distribution by Load Bucket (Up to 40,000) for", node_df$city[1], "in", year_of_data),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Locational Marginal Price ($/MWh)",
      fill = "Median LMP"
    ) +
    ggthemes::theme_solarized() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(p)
}


#' Visualize Node Data: Congestion
#' 
#' This function takes in a tibble of a year's worth of 15-minute data for a node and visualizes the data with a box and whisker plot of congestion grouped by load buckets.
#' 
#' @param node_df A dataframe of locational marginal prices for a year for one node. The default for this is San Francisco 2023, but a user can input a different dataframe by running the `pulldata_node` function.
#' 
#' @return A `ggplot2` object displaying the distribution of congestion by demand bucket (up to 40,000).
#' 
#' @export
visualize_node_demand_congestion <- function(node_df = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR"))) {
  node_df = merge_demand(node_df)
  
  # Create load buckets ranging from 12,000 to 40,000 with 1,000 intervals
  node_df <- node_df |>
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      load_bucket = cut(load, 
                        breaks = seq(12000, 52000, by = 1000), 
                        include.lowest = TRUE,
                        labels = paste0(seq(12, 51, by = 1), "-", seq(13, 52, by = 1)))
    )
  
  # Summarize congestion data by load bucket
  avg_congestion_per_bucket <- node_df |>
    dplyr::group_by(load_bucket) |>
    dplyr::summarise(
      lower_whisker = quantile(congestion, 0.1, na.rm = TRUE),
      lower_quartile = quantile(congestion, 0.25, na.rm = TRUE),
      median = median(congestion, na.rm = TRUE),
      upper_quartile = quantile(congestion, 0.75, na.rm = TRUE),
      upper_whisker = quantile(congestion, 0.9, na.rm = TRUE)
    )
  
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(lubridate::ymd_hms(interval_time), "%Y")
  
  # Plot the results
  p <- ggplot2::ggplot(avg_congestion_per_bucket, ggplot2::aes(x = load_bucket)) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = as.numeric(factor(load_bucket)) - 0.4,
        xmax = as.numeric(factor(load_bucket)) + 0.4,
        ymin = lower_quartile,
        ymax = upper_quartile
      ),
      fill = "steelblue",
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(load_bucket)),
        xend = as.numeric(factor(load_bucket)),
        y = lower_whisker,
        yend = lower_quartile
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(load_bucket)),
        xend = as.numeric(factor(load_bucket)),
        y = upper_quartile,
        yend = upper_whisker
      ),
      color = "black"
    ) +
    ggplot2::geom_point(ggplot2::aes(
      x = as.numeric(factor(load_bucket)),
      y = median
    ), color = "black", size = .75) +
    ggplot2::scale_x_discrete(
      name = "Load Buckets (Thousand MWs)",
      labels = levels(avg_congestion_per_bucket$load_bucket)
    ) +
    ggplot2::labs(
      title = paste("Congestion Distribution by Load Bucket (Up to 40,000) for", node_df$city[1], "in", year_of_data),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Congestion ($/MWh)",
      fill = "Median Congestion"
    ) +
    ggthemes::theme_solarized() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(p)
}




#' Visualize Node Data Without High Demand Hours: Congestion
#' 
#' This function takes in a tibble of a year's worth of 15-minute data for a node and visualizes the data with a box and whisker plot of congestion grouped by load buckets, excluding high-demand hours.
#' 
#' @param node_df A dataframe of locational marginal prices for a year for one node. The default for this is San Francisco 2023, but a user can input a different dataframe by running the `pulldata_node` function.
#' 
#' @return A `ggplot2` object displaying the distribution of congestion by demand bucket (up to 40,000).
#' 
#' @export
visualize_node_demand_without_high_demand_hours_congestion <- function(node_df = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package = "CAISOWrapR"))) {
  node_df = merge_demand(node_df)
  
  # Filter out high-demand loads
  node_df <- node_df |> dplyr::filter(load <= 40000)
  
  # Create load buckets ranging from 12,000 to 40,000 with 1,000 intervals
  node_df <- node_df |>
    dplyr::mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      load_bucket = cut(load, 
                        breaks = seq(12000, 40000, by = 1000), 
                        include.lowest = TRUE,
                        labels = paste0(seq(12, 39, by = 1), "-", seq(13, 40, by = 1)))
    )
  
  # Summarize congestion data by load bucket
  avg_congestion_per_bucket <- node_df |>
    dplyr::group_by(load_bucket) |>
    dplyr::summarise(
      lower_whisker = quantile(congestion, 0.1, na.rm = TRUE),
      lower_quartile = quantile(congestion, 0.25, na.rm = TRUE),
      median = median(congestion, na.rm = TRUE),
      upper_quartile = quantile(congestion, 0.75, na.rm = TRUE),
      upper_whisker = quantile(congestion, 0.9, na.rm = TRUE)
    )
  
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(lubridate::ymd_hms(interval_time), "%Y")
  
  # Plot the results
  p <- ggplot2::ggplot(avg_congestion_per_bucket, ggplot2::aes(x = load_bucket)) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = as.numeric(factor(load_bucket)) - 0.4,
        xmax = as.numeric(factor(load_bucket)) + 0.4,
        ymin = lower_quartile,
        ymax = upper_quartile
      ),
      fill = "steelblue",
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(load_bucket)),
        xend = as.numeric(factor(load_bucket)),
        y = lower_whisker,
        yend = lower_quartile
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = as.numeric(factor(load_bucket)),
        xend = as.numeric(factor(load_bucket)),
        y = upper_quartile,
        yend = upper_whisker
      ),
      color = "black"
    ) +
    ggplot2::geom_point(ggplot2::aes(
      x = as.numeric(factor(load_bucket)),
      y = median
    ), color = "black", size = .75) +
    ggplot2::scale_x_discrete(
      name = "Load Buckets (Thousand MWs)",
      labels = levels(avg_congestion_per_bucket$load_bucket)
    ) +
    ggplot2::labs(
      title = paste("Congestion Distribution by Load Bucket (Up to 40,000) for", node_df$city[1], "in", year_of_data),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Congestion ($/MWh)",
      fill = "Median Congestion"
    ) +
    ggthemes::theme_solarized() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(p)
}

