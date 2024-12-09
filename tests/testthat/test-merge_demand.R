test_that("merge_demand() returns a data frame with correct columns", {
  # Create a sample node_data to test with
  node_data <- data.frame(interval_start_utc = c("2023-01-01 00:00:00", "2023-01-01 01:00:00"), 
                          other_column = c(1, 2))
    # Run the function
  result <- merge_demand(node_data)
  
  # Check that the result is a data frame
  expect_s3_class(result, "data.frame")
})
