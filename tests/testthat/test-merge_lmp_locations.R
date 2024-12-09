test_that("merge_lmp_locations() merges LMP and location data correctly", {
  # Create a sample instance dataframe
  instance <- data.frame(location = c("San Francisco", "Los Angeles"),
                         lmp = c(50, 100))
  
  # Create a mock location_data (cities_and_lmps.csv content)
  location_data <- data.frame(closest_lmp = c("San Francisco", "Los Angeles"),
                              name = c("San Francisco", "Los Angeles"),
                              closest_distance = c(10, 20))
  
  # Mock the read.csv function to return the mock location_data
  mock_read_csv <- function(file) {
    return(location_data)
  }
  # Temporarily replace read.csv with mock_read_csv
  assign("read.csv", mock_read_csv, envir = globalenv())
  
  # Run the function
  result <- merge_lmp_locations(instance)
  
  # Check that the result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check that the city column is correctly renamed
  expect_true("San Francisco" %in% result$city)
})

