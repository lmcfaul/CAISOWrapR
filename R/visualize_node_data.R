# Load required library
library(dplyr)

# Create a sequence of time intervals (15-minute intervals for one year)
start_time <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
end_time <- as.POSIXct("2024-12-31 23:45:00", tz = "UTC")
time_intervals <- seq(from = start_time, to = end_time, by = "15 min")

# Generate a data frame with the required columns
df <- data.frame(
  interval_start_utc = time_intervals,
  interval_end_utc = time_intervals + 15 * 60,  # 15 minutes later
  market = "CAISO",  # Example market name, you can vary this if needed
  location = "Location_A",  # Example location
  location_type = "Type_A",  # Example location type
  lmp = runif(length(time_intervals), min = 20, max = 100),  # Random LMP values
  energy = runif(length(time_intervals), min = 100, max = 1000),  # Random energy values
  congestion = runif(length(time_intervals), min = 0, max = 50),  # Random congestion values
  loss = runif(length(time_intervals), min = 0, max = 10)  # Random loss values
)

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
  
  # Calculate average price per time
  avg_price_per_time <- df %>%
    group_by(interval_start_utc) %>%
    summarise(avg_lmp = mean(lmp)) %>%
    mutate(plot_type = "Average Price per Time")
  
  # Calculate average congestion per time
  avg_congestion_per_time <- df %>%
    group_by(interval_start_utc) %>%
    summarise(avg_congestion = mean(congestion)) %>%
    mutate(plot_type = "Average Congestion per Time")
  
  # Combine the data for price over time
  price_over_time <- df %>%
    mutate(plot_type = "Price over Time")
  
  # Combine the data for congestion over time
  congestion_over_time <- df %>%
    mutate(plot_type = "Congestion over Time")
  
  # Combine all data into one dataframe
  combined_df <- bind_rows(
    avg_price_per_time %>% select(interval_start_utc, avg_lmp, plot_type),
    avg_congestion_per_time %>% select(interval_start_utc, avg_congestion, plot_type),
    price_over_time %>% select(interval_start_utc, lmp, plot_type),
    congestion_over_time %>% select(interval_start_utc, congestion, plot_type)
  )
  
  # Rename the columns for easier plotting and to avoid name duplication
  combined_df <- combined_df %>%
    rename(
      time = interval_start_utc,
      price = avg_lmp,
      congestion_avg = avg_congestion
    ) %>%
    mutate(
      price = ifelse(is.na(price), lmp, price),  # Replace NA in price with lmp
      congestion_avg = ifelse(is.na(congestion_avg), congestion, congestion_avg)  # Replace NA in congestion_avg with congestion
    )
  
  # Plot using ggplot with facets
  p <- ggplot(combined_df, aes(x = time)) +  # Correct function here: ggplot, not ggplot2
    geom_line(aes(y = price, color = plot_type)) +  # Plot price or congestion
    geom_line(aes(y = congestion_avg, color = plot_type), linetype = "dashed") +  # Plot dashed congestion line
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
