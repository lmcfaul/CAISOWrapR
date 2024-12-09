# CAISOWrapR:

## Installation:

To install CAISOWrapR, simply run `install.packages()` to install from CRAN:

```{r}
install.packages("CAISOWrapR")
```

## How to Use the CAISO API Wrapper:

The **CAISO API Client** provides a robust R interface for accessing and analyzing CAISO Locational Marginal Pricing (LMP) data via the Grid Status API. Follow these instructions to get started and access LMP data:

### Setting Up the Client

In order to set up the CAISO API Client, you must first have an API key from the Grid Status API. If you do not have an API key, you can retrieve a free one from here: [Grid Status API](https://www.gridstatus.io/api).

Once you have the you can use the `create_caiso_client()` function to create the client.

```{r}
library(CAISOWrapR)

# Create API client
client <- create_caiso_client(api_key = "my_api_key")
```

Alternatively, you can set your API key as the environment variable `GRIDSTATUS_API_KEY` in your R session.

```{r}
# Set API key as an environment variable
Sys.setenv(GRIDSTATUS_API_KEY = "my_api_key")

# Create API client
client <- create_caiso_client()
```

### Fetching Data

To retrieve CAISO's LMP data, you can use the `fetch_lmp_data()` function

```{r}
# Retrieve 15-minute real-time LMP data for a specific date range
lmp_data <- fetch_lmp_data(
  client,
  dataset = "caiso_lmp_real_time_15_min",
  start = "2024-01-01 00:00:00",
  end = "2024-01-02 00:00:00",
  columns = c("interval_start_utc", "lmp", "location"),
  limit = 10
)
```

#### Advanced Options:

-   **Filtering:** Apply filters to query specific data points:

    ```{r}
    # Retrieve LMP data with filtering for specific column
    filtered_data <- fetch_lmp_data(
      client,
      limit = 10,
      filter_column = "location",
      filter_value = "12STREET_LNODEEET1",
      filter_operator = "="
    )
    ```

-   **Resampling:** Aggregate data to a different frequency using resampling:

    ```{r}
    # Retrieve LMP data with resampling
    resampled_data <- fetch_lmp_data(
      client,
      limit = 10,
      resample = "1H",  # Hourly
      resample_function = "mean",
      resample_by = c("location", "market")
    )
    ```

## Next Section

## How to use CA_map() function

The CA_map() function is a simple function that allows you to visualize the LMP data on a map of California. The function takes in a data frame of one instance of LMP data and plots the LMP values for each location on a map of California. The user can then visualize LMPs, congestion, losses, and transmission lines.

The user can choose to input a data frame into the function, use argument "peak_load" (September 6, 2022 at 5 PM), or use the default instance of data, which is an average day in California at 1pm. 
### Example:
```{r}
# Plot LMP data on a map of California
CA_map()
```

## Next Section

## How to use pulldata_instance() function

The pulldata_instance() function is a simple function that allows you to pull LMP data for a specific instance of time. The function takes in a data, time, and API key and returns the LMP data for that specific instance of time. The user can then visualize LMPs, congestion, losses, and transmission lines through other functions.

## Next Section

## How to use pulldata_node() function

The pulldata_instance() function is a simple function that allows you to pull LMP data for a specific location in California over time. The function takes in a city or node, a year, and an API key and returns the LMP data for that specific location over time. The user can then visualize LMPs, congestion, losses, and transmission lines through other functions. The time intervals are 15 minutes.

## Next Section

## How to use analyze_single_node_data() function

This function is allows a user to pass in a dataframe of one node prices over time (created through pulldata_node function) and exports a pdf with graphs and analysis. There are many functions a user can run to analyze the data, such as graphing it over time, by season, and in combination with electricity demand. This function runs all of those analsyses and returns a formatted pdf with the results. These results are for one node (location) over a year.

## Next Section

## How to use analyze_two_node_data() function

This function is allows a user to pass in two dataframes of node prices over time for two different locations (created through pulldata_node function) and exports a pdf with graphs and analysis. There are many functions a user can run to analyze the data, such as graphing it over time, by season, and in combination with electricity demand. This function runs all of those analyses and returns a formatted pdf with the results. These results are for two nodes (locations) over a year.
