# This script has all of the functions that analyze a single node of data (one location with all 15 minute intervals for a year) and combines the results into a pdf to export. The functions `visualize_node` and `visualize_node_seasonal` are called with an inputting dataframe of a node of data. The user has to call the `pulldata_node` function to get the data for the node they want to analyze. The user can also just call the individual analysis functions if they want to see the results separately.


#' Analyze Single Node Data
#' 
#' This function calls the functions `visualize_node` and `visualize_node_seasonal` with an inputting dataframe of a node of data (one location over a year) and combines the results into a pdf to export. First, the user has to call the `pulldata_node` function to get the data for the node they want to analyze.
#' 
#' @param node_df A dataframe of locational marginal prices for a year for one node
#' @param output_pdf_path The path to save the pdf file. There is a default path of "single_instance_data.pdf" but the user can edit this path to save the file in a different location.
#' 
#' @return A pdf file with the two plots
#' 
#' @import grid
#' @import gridExtra
#' @import ggplot2
#' @import lubridate
#' @import gtable
#' @import dplyr
#' 
#' @export
analyze_single_node_data <- function(node_df, output_pdf_path = "single_node_data.pdf") {
  # Visualizations for LMP
  result_one <- visualize_node(node_df)
  result_two <- visualize_node_seasonal(node_df)
  
  # Visualizations for congestion
  result_three <- visualize_node_congestion(node_df)
  result_four <- visualize_node_seasonal_congestion(node_df)
  
  # Visualizations for Loss Pricing Adjustments
  result_five <- visualize_node_losses(node_df)
  result_six <- visualize_node_seasonal_losses(node_df)
  
  # Extract the year and city for the title
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(ymd_hms(interval_time), "%Y")
  
  title <- textGrob(
    paste("LMP Distributions for", node_df$city[1], "in", year_of_data),
    gp = gpar(fontsize = 16, fontface = "bold"),
    just = "center"
  )
  
  # Add margins to each plot
  plot_with_margins_1 <- gtable_add_padding(
    ggplotGrob(result_one),
    padding = unit(c(1, 1, 1, 1), "cm")
  )
  
  plot_with_margins_2 <- gtable_add_padding(
    ggplotGrob(result_two),
    padding = unit(c(1, 1, 1, 1), "cm")
  )
  
  plot_with_margins_3 <- gtable_add_padding(
    ggplotGrob(result_three),
    padding = unit(c(1, 1, 1, 1), "cm")
  )
  
  plot_with_margins_4 <- gtable_add_padding(
    ggplotGrob(result_four),
    padding = unit(c(1, 1, 1, 1), "cm")
  )
  
  plot_with_margins_5 <- gtable_add_padding(
    ggplotGrob(result_five),
    padding = unit(c(1, 1, 1, 1), "cm")
  )
  
  plot_with_margins_6 <- gtable_add_padding(
    ggplotGrob(result_six),
    padding = unit(c(1, 1, 1, 1), "cm")
  )
  
  # Create the PDF file
  pdf(output_pdf_path, width = 8.5, height = 11)
  
  # First page: LMP plots
  grid.arrange(
    title,
    plot_with_margins_1,
    plot_with_margins_2,
    nrow = 3,
    heights = c(0.1, 0.45, 0.45)
  )
  
  # Second page: Congestion plots
  grid.arrange(
    textGrob(
      paste("Congestion for", node_df$city[1], "in", year_of_data),
      gp = gpar(fontsize = 16, fontface = "bold"),
      just = "center"
    ),
    plot_with_margins_3,
    plot_with_margins_4,
    nrow = 3,
    heights = c(0.1, 0.45, 0.45)
  )
  
  # Third page: Loss Pricing Adjustments plots
  grid.arrange(
    textGrob(
      paste("Loss Pricing Adjustments for", node_df$city[1], "in", year_of_data),
      gp = gpar(fontsize = 16, fontface = "bold"),
      just = "center"
    ),
    plot_with_margins_5,
    plot_with_margins_6,
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
#' over a day
#' 
#' @param node_df a dataframe of a year's worth of 15-minute data for a node
#' 
#' @return 1 graph
#' 
#' @import ggplot2
#' @import dplyr
#' @import lubridate with_tz
#' @import ggthemes theme_solarized
#' 
#' @export
visualize_node = function(node_df){
  
  node_df <- node_df %>%
    mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S")
    )
  
  avg_price_per_time <- node_df %>%
    group_by(time_only) %>%
    summarise(
      lower_whisker = quantile(lmp, 0.1, na.rm = TRUE),
      lower_quartile = quantile(lmp, 0.25, na.rm = TRUE),
      median = median(lmp, na.rm = TRUE),
      upper_quartile = quantile(lmp, 0.75, na.rm = TRUE),
      upper_whisker = quantile(lmp, 0.9, na.rm = TRUE)
    )
  
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(ymd_hms(interval_time), "%Y")
  
  p <- ggplot(avg_price_per_time, aes(x = time_only)) +
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
    geom_point(aes(
      x = as.numeric(factor(time_only)),
      y = median
    ), color = "black", size = .75) +
    scale_x_discrete(
      name = "Hour of the Day",
      breaks = avg_price_per_time$time_only[avg_price_per_time$time_only %in% sprintf("%02d:00:00", seq(0, 22, 2))],
      labels = sprintf("%02d:00", seq(0, 22, 2))
    ) +
    labs(
      title = paste("LMP Distribution by 15-minute Interval for", node_df$city[1], "in", year_of_data), 
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Locational Marginal Price ($/MWh)",
      fill = "Median LMP"
    ) +
    theme_solarized() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  return(p)
  
}

#' Visualize distribution of LMPs in one location by season
#' 
#' This script reads in a dataframe of LMP data for a city over a year and visualizes the distribution of LMPs by hour for each season.
#' 
#' @param node_df A tibble containing LMP data for the first city over a year
#' 
#' @return A ggplot object displaying the distribution of LMPs by hour for the location over a day
#' 
#' @import dplyr
#' @import ggplot2
#' @import lubridate with_tz
#' @import ggthemes theme_solarized
#' 
#' @export
visualize_node_seasonal = function(node_df){
  
  node_df <- node_df %>%
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
    filter(!is.na(season))
  
  avg_price_per_time <- node_df %>%
    group_by(hour, season) %>%
    summarise(
      lower_whisker = quantile(lmp, 0.1, na.rm = TRUE),
      lower_quartile = quantile(lmp, 0.25, na.rm = TRUE),
      median = median(lmp, na.rm = TRUE),
      upper_quartile = quantile(lmp, 0.75, na.rm = TRUE),
      upper_whisker = quantile(lmp, 0.9, na.rm = TRUE),
      .groups = "drop"
    )
  
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(ymd_hms(interval_time), "%Y")
  
  x_breaks <- seq(0, 23, by = 2)
  x_labels <- sprintf("%02d:00", x_breaks)
  
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
      title = paste("Seasonal LMP Distribution by Hour for", node_df$city[1], "in", year_of_data),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Locational Marginal Price ($/MWh)",
      fill = "Median LMP"
    ) +
    facet_wrap(~season, scales = "fixed", ncol = 2) + 
    theme_solarized() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5)
    )
  
  return(p)
  
}


#' Visualize Node Data: Congestion
#' 
#' This function takes in a dataframe of a year's worth of 15-minute data for a node
#' and visualizes the data in a box and whisker plot by interval to see how the price distribution ranges
#' over a day
#' 
#' @param node_df a dataframe of a year's worth of 15-minute data for a node
#' 
#' @return 1 graph
#' 
#' @import ggplot2
#' @import dplyr
#' @import lubridate with_tz
#' @import ggthemes theme_solarized
#' 
#' @export
visualize_node_congestion = function(node_df){
  
  node_df <- node_df %>%
    mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S")
    )
  
  avg_price_per_time <- node_df %>%
    group_by(time_only) %>%
    summarise(
      lower_whisker = quantile(congestion, 0.1, na.rm = TRUE),
      lower_quartile = quantile(congestion, 0.25, na.rm = TRUE),
      median = median(congestion, na.rm = TRUE),
      upper_quartile = quantile(congestion, 0.75, na.rm = TRUE),
      upper_whisker = quantile(congestion, 0.9, na.rm = TRUE)
    )
  
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(ymd_hms(interval_time), "%Y")
  
  p <- ggplot(avg_price_per_time, aes(x = time_only)) +
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
    geom_point(aes(
      x = as.numeric(factor(time_only)),
      y = median
    ), color = "black", size = .75) +
    scale_x_discrete(
      name = "Hour of the Day",
      breaks = avg_price_per_time$time_only[avg_price_per_time$time_only %in% sprintf("%02d:00:00", seq(0, 22, 2))],
      labels = sprintf("%02d:00", seq(0, 22, 2))
    ) +
    labs(
      title = paste("Congestion Price Distribution by 15-minute Interval for", node_df$city[1], "in", year_of_data), 
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Congestion Price ($/MWh)",
      fill = "Median Congestion Price"
    ) +
    theme_solarized() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  return(p)
  
}


#' Visualize distribution of Congestion prices in one location by season
#' 
#' This script reads in a dataframe of LMP data for a city over a year and visualizes the distribution of LMPs by hour for each season.
#' 
#' @param node_df A tibble containing LMP data for the first city over a year
#' 
#' @return A ggplot object displaying the distribution of LMPs by hour for the location over a day
#' 
#' @import dplyr
#' @import ggplot2
#' @import lubridate with_tz
#' @import ggthemes theme_solarized
#' 
#' @export
visualize_node_seasonal_congestion = function(node_df){
  
  node_df <- node_df %>%
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
    filter(!is.na(season))
  
  avg_price_per_time <- node_df %>%
    group_by(hour, season) %>%
    summarise(
      lower_whisker = quantile(congestion, 0.1, na.rm = TRUE),
      lower_quartile = quantile(congestion, 0.25, na.rm = TRUE),
      median = median(congestion, na.rm = TRUE),
      upper_quartile = quantile(congestion, 0.75, na.rm = TRUE),
      upper_whisker = quantile(congestion, 0.9, na.rm = TRUE),
      .groups = "drop"
    )
  
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(ymd_hms(interval_time), "%Y")
  
  x_breaks <- seq(0, 23, by = 2)
  x_labels <- sprintf("%02d:00", x_breaks)
  
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
      title = paste("Seasonal Congestion Price Distribution by Hour for", node_df$city[1], "in", year_of_data),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Congestion Price ($/MWh)",
      fill = "Median Congestion Price"
    ) +
    facet_wrap(~season, scales = "fixed", ncol = 2) + 
    theme_solarized() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5)
    )
  
  return(p)
  
}

#' Visualize Node Data: Loss Pricing Adjustment
#' 
#' This function takes in a dataframe of a year's worth of 15-minute data for a node
#' and visualizes the data in a box and whisker plot by interval to see how the price distribution ranges
#' over a day
#' 
#' @param node_df a dataframe of a year's worth of 15-minute data for a node
#' 
#' @return 1 graph
#' 
#' @import ggplot2
#' @import dplyr
#' @import lubridate with_tz
#' @import ggthemes theme_solarized
#' 
#' @export
visualize_node_losses = function(node_df){
  
  node_df <- node_df %>%
    mutate(
      interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
      time_only = format(interval_start_pst, "%H:%M:%S")
    )
  
  avg_price_per_time <- node_df %>%
    group_by(time_only) %>%
    summarise(
      lower_whisker = quantile(loss, 0.1, na.rm = TRUE),
      lower_quartile = quantile(loss, 0.25, na.rm = TRUE),
      median = median(loss, na.rm = TRUE),
      upper_quartile = quantile(loss, 0.75, na.rm = TRUE),
      upper_whisker = quantile(loss, 0.9, na.rm = TRUE)
    )
  
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(ymd_hms(interval_time), "%Y")
  
  p <- ggplot(avg_price_per_time, aes(x = time_only)) +
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
    geom_point(aes(
      x = as.numeric(factor(time_only)),
      y = median
    ), color = "black", size = .75) +
    scale_x_discrete(
      name = "Hour of the Day",
      breaks = avg_price_per_time$time_only[avg_price_per_time$time_only %in% sprintf("%02d:00:00", seq(0, 22, 2))],
      labels = sprintf("%02d:00", seq(0, 22, 2))
    ) +
    labs(
      title = paste("Loss Pricing Adjustment Distribution by 15-minute Interval for", node_df$city[1], "in", year_of_data), 
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Loss Pricing Adjustment ($/MWh)",
      fill = "Median Loss Pricing Adjustment"
    ) +
    theme_solarized() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  return(p)
  
}



#' Visualize distribution of Loss pricing adjustments in one location by season
#' 
#' This script reads in a dataframe of LMP data for a city over a year and visualizes the distribution of LMPs by hour for each season.
#' 
#' @param node_df A tibble containing LMP data for the first city over a year
#' 
#' @return A ggplot object displaying the distribution of LMPs by hour for the location over a day
#' 
#' @import dplyr
#' @import ggplot2
#' @import lubridate with_tz
#' @import ggthemes theme_solarized
#' 
#' @export
visualize_node_seasonal_losses = function(node_df){
  
  node_df <- node_df %>%
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
    filter(!is.na(season))
  
  avg_price_per_time <- node_df %>%
    group_by(hour, season) %>%
    summarise(
      lower_whisker = quantile(loss, 0.1, na.rm = TRUE),
      lower_quartile = quantile(loss, 0.25, na.rm = TRUE),
      median = median(loss, na.rm = TRUE),
      upper_quartile = quantile(loss, 0.75, na.rm = TRUE),
      upper_whisker = quantile(loss, 0.9, na.rm = TRUE),
      .groups = "drop"
    )
  
  interval_time <- unique(node_df$interval_start_utc)[1]
  year_of_data <- format(ymd_hms(interval_time), "%Y")
  
  x_breaks <- seq(0, 23, by = 2)
  x_labels <- sprintf("%02d:00", x_breaks)
  
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
      title = paste("Seasonal Loss Pricing Adjustment Distribution by Hour for", node_df$city[1], "in", year_of_data),
      subtitle = "Median, Interquartile Range, and 10% and 90% whiskers",
      y = "Loss Pricing Adjustment ($/MWh)",
      fill = "Median Loss Pricing Adjustment"
    ) +
    facet_wrap(~season, scales = "fixed", ncol = 2) + 
    theme_solarized() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5)
    )
  
  return(p)
  
}

