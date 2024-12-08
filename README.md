# CAISOWrapR: 

## Installation

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
