# Mock objects and helper functions for testing
create_mock_client <- function() {
  structure(
    list(
      host = "https://api.gridstatus.io/v1",
      api_key = "test_api_key",
      max_retries = 3
    ),
    class = "CAISOClient"
  )
}

# Test create_caiso_client function
test_that("create_caiso_client works with valid inputs", {
  # Set a temporary API key
  withr::local_envvar(GRIDSTATUS_API_KEY = "test_key")
  
  # Test with default parameters
  client <- create_caiso_client()
  expect_s3_class(client, "CAISOClient")
  expect_equal(client$host, "https://api.gridstatus.io/v1")
  expect_equal(client$max_retries, 3)
  
  # Test with custom parameters
  custom_client <- create_caiso_client(
    host = "https://custom-api.example.com", 
    api_key = "custom_key",
    max_retries = 5
  )
  expect_equal(custom_client$host, "https://custom-api.example.com")
  expect_equal(custom_client$api_key, "custom_key")
  expect_equal(custom_client$max_retries, 5)
})

test_that("create_caiso_client handles API key errors", {
  # Temporarily unset any existing API key
  withr::local_envvar(GRIDSTATUS_API_KEY = "")
  
  # Expect an error when no API key is provided
  expect_error(
    create_caiso_client(),
    regexp = "No API key provided"
  )
})

# Test validate_client function
test_that("validate_client correctly validates client objects", {
  # Valid client
  valid_client <- create_mock_client()
  expect_true(validate_client(valid_client))
  
  # Invalid clients
  expect_error(validate_client(list()), "Invalid client")
  
  incomplete_client <- structure(
    list(host = "https://example.com"),
    class = "CAISOClient"
  )
  expect_error(validate_client(incomplete_client), "Client is missing required configuration")
})

# Test compact_list function
test_that("compact_list removes NULL elements", {
  test_list <- list(a = 1, b = NULL, c = 2, d = NULL)
  compacted <- compact_list(test_list)
  
  expect_equal(names(compacted), c("a", "c"))
  expect_equal(compacted$a, 1)
  expect_equal(compacted$c, 2)
})

# Test extract_first function
test_that("extract_first handles API data", {
  expect_equal(extract_first(list(list(1), list(2))), list(1, 2))
  expect_true(is.na(extract_first(NULL)))
})

# Test convert_lmp_datatypes function
test_that("convert_lmp_datatypes handles type conversion", {
  # Create a sample data frame
  sample_data <- data.frame(
    interval_start_utc = I(list("2024-01-01T12:00:00")),
    interval_end_utc = I(list("2024-01-01T13:00:00")),
    market = I(list("DAY_AHEAD")),
    location = I(list("PALO_VERDE")),
    location_type = I(list("HUB")),
    lmp = I(list("50.25")),
    energy = I(list("30.50")),
    congestion = I(list("15.75")),
    loss = I(list("4.00"))
  )
  
  converted_data <- convert_lmp_datatypes(sample_data)
  
  # Check column types
  expect_s3_class(converted_data$interval_start_utc, "POSIXct")
  expect_s3_class(converted_data$market, "factor")
  expect_s3_class(converted_data$location, "factor")
  expect_type(converted_data$lmp, "double")
})


