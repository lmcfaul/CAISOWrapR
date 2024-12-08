df1 <- stanford_year %>%
  mutate(
    # Convert to POSIXct in UTC
    interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    # Convert to PST (Pacific Time)
    interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
    # Extract time-only portion in PST
    time_only = format(interval_start_pst, "%H:%M:%S"),
    hour = as.numeric(substr(time_only, 1, 2))  # Extract hour
  )

df2 <- san_diego_year %>%
  mutate(
    # Convert to POSIXct in UTC
    interval_start_utc = as.POSIXct(interval_start_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    # Convert to PST (Pacific Time)
    interval_start_pst = lubridate::with_tz(interval_start_utc, tzone = "America/Los_Angeles"),
    # Extract time-only portion in PST
    time_only = format(interval_start_pst, "%H:%M:%S"),
    hour = as.numeric(substr(time_only, 1, 2))
  )


avg_price_per_time1 <- df1 %>%
  filter(time_only == paste0(sprintf("%02d", hour), ":00:00")) %>%  # Keep only the first instance for each hour
  group_by(hour) %>%
  summarise(
    lower_whisker = quantile(lmp, 0.1, na.rm = TRUE),
    lower_quartile = quantile(lmp, 0.25, na.rm = TRUE),
    median = median(lmp, na.rm = TRUE),
    upper_quartile = quantile(lmp, 0.75, na.rm = TRUE),
    upper_whisker = quantile(lmp, 0.9, na.rm = TRUE),
    city = pull(df1, city)[1]  # Assign city
  )

avg_price_per_time2 <- df2 %>%
  filter(time_only == paste0(sprintf("%02d", hour), ":00:00")) %>%  # Keep only the first instance for each hour
  group_by(hour) %>%
  summarise(
    lower_whisker = quantile(lmp, 0.1, na.rm = TRUE),
    lower_quartile = quantile(lmp, 0.25, na.rm = TRUE),
    median = median(lmp, na.rm = TRUE),
    upper_quartile = quantile(lmp, 0.75, na.rm = TRUE),
    upper_whisker = quantile(lmp, 0.9, na.rm = TRUE),
    city = pull(df2, city)[1]  # Assign city
  )

city1 <- unique(avg_price_per_time1$city)  # City from df1
city2 <- unique(avg_price_per_time2$city)  # City from df2

# Plot
q <- ggplot() +
  # Draw the interquartile range as "boxes" for the first dataset
  geom_rect(
    data = avg_price_per_time1,
    aes(
      xmin = hour - 0.35,  # Shift left
      xmax = hour - 0.005,  # Narrow box width
      ymin = lower_quartile,
      ymax = upper_quartile,
      fill = city
    ),
    alpha = 0.5
  ) +
  # Draw whiskers for 10th and 90th percentiles (first dataset)
  geom_segment(
    data = avg_price_per_time1,
    aes(
      x = hour - 0.2,  # Align with blue box
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
  # Add points for the median (first dataset)
  geom_point(
    data = avg_price_per_time1,
    aes(
      x = (hour - 0.35 + hour - 0.005) / 2,
      y = median,
    ),
    size = 0.75,
    color = "black"
  ) +
  # Draw the interquartile range as "boxes" for the second dataset
  geom_rect(
    data = avg_price_per_time2,
    aes(
      xmin = hour + 0.05,  # Shift right
      xmax = hour + 0.35,  # Narrow box width
      ymin = lower_quartile,
      ymax = upper_quartile,
      fill = city
    ),
    alpha = 0.5
  ) +
  # Draw whiskers for 10th and 90th percentiles (second dataset)
  geom_segment(
    data = avg_price_per_time2,
    aes(
      x = hour + 0.2,  # Align with red box
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
  # Add points for the median (second dataset)
  geom_point(
    data = avg_price_per_time2,
    aes(
      x = hour + 0.2,
      y = median,
    ),
    color = "black",
    size = 0.75
  ) +
  # Add labels and customize theme
  scale_x_continuous(
    name = "Hour of the Day",
    breaks = seq(0, 23, 2),  # Breaks for each hour
    labels = sprintf("%02d:00", seq(0, 23, 2))  # Display in HH:00 format
  ) +
  scale_fill_manual(
    name = "City",
    values = c("steelblue", "red")  # Ensure consistent color mapping
  ) +
  scale_color_manual(
    name = "City",
    values = c("steelblue", "red")  # Ensure consistent color mapping
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

# Display the plot
q

