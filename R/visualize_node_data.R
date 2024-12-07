#' Visualize Node Data
#' 
#' This function takes in a dataframe of a year's worth of 15-minute data for a node
#' and visualizes the data in 4 graphs: average price per time (line chart),
#' price over time (line chart), average congestion per time (line chart), and
#' congestion over time (line chart).
#' 
#' @param df a dataframe of a year's worth of 15-minute data for a node
#' 
#' @return 1 graph: facet grid of 4 line charts
#' 
#' @import ggplot2
#' @import dplyr
#' 
#' @export
visualize_node = function(df){
  
  # Create a new column for the time portion of interval_start_utc
  df <- df %>%
    mutate(time_only = format(as.POSIXct(interval_start_utc), "%H:%M:%S"))  # Extract only time (HH:MM:SS)
  
  # Calculate average price per time (group by the time only)
  avg_price_per_time <- df %>%
    group_by(time_only) %>%
    summarise(avg_lmp = mean(lmp, na.rm = TRUE)) %>%
    mutate(plot_type = "Average Price per Time")
  
  # Calculate average congestion per time (group by the time only)
  avg_congestion_per_time <- df %>%
    group_by(time_only) %>%
    summarise(avg_congestion = mean(congestion, na.rm = TRUE)) %>%
    mutate(plot_type = "Average Congestion per Time")
  
  # Combine the data for price over time (using original time stamps)
  price_over_time <- df %>%
    mutate(plot_type = "Price over Time")
  
  # Combine the data for congestion over time (using original time stamps)
  congestion_over_time <- df %>%
    mutate(plot_type = "Congestion over Time")
  
  # Plot using ggplot with facets
  p <- ggplot() +
    # Plot average price per time
    geom_line(data = avg_price_per_time, aes(x = time_only, y = avg_lmp, color = "Average Price per Time")) +
    # Plot average congestion per time
    geom_line(data = avg_congestion_per_time, aes(x = time_only, y = avg_congestion, color = "Average Congestion per Time")) +
    # Plot price over time
    geom_line(data = price_over_time, aes(x = time_only, y = lmp, color = "Price over Time")) +
    # Plot congestion over time
    geom_line(data = congestion_over_time, aes(x = time_only, y = congestion, color = "Congestion over Time")) +
    facet_wrap(~ plot_type, scales = "free_y") +  # Facet grid with separate y-axes
    labs(x = "Time", y = "Value", title = "Node Data Visualization") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      legend.position = "none"  # Remove the legend since it's redundant
    )
  
  # Return the plot
  return(p)
}

p = ggplot() + 
  geom_col(data = testing_palo_alto, aes(x = time_only, y = avg_lmp, fill = "Average Price per Time")) +
  labs(title = "Average Price per Time", x = "Time", y = "Average LMP") +
  theme_minimal()

p

