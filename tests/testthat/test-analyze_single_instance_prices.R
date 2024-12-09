test_that("summarize_lmps() returns a dataframe", {
  # Create a sample dataframe
  instance_df <- data.frame(
    lmp = c(20.5, 30.2, 25.1),
    location = c("Loc1", "Loc2", "Loc3")
  )
  
  # Run the function
  result <- summarize_lmps(instance_df)
  
  # Check that the result is a dataframe
  expect_s3_class(result, "data.frame")
})

test_that("price_violin() returns a ggplot object", {
  # Create a sample dataframe
  instance_df <- data.frame(
    lmp = c(20.5, 30.2, 25.1),
    location = c("Loc1", "Loc2", "Loc3")
  )
  
  # Run the function
  result <- price_violin(instance_df)
  
  # Check that the result is a ggplot object
  expect_s3_class(result, "gg")
})

test_that("analyze_single_instance_data() produces a PDF", {
  # Create a mock file path
  output_pdf_path <- tempfile(fileext = ".pdf")
  
  # Create a sample dataframe
  instance_df <- data.frame(
    lmp = c(20.5, 30.2, 25.1),
    location = c("Loc1", "Loc2", "Loc3"),
    interval_start_utc = c("2023-01-01 13:00:00", "2023-01-01 13:15:00", "2023-01-01 13:30:00")
  )
  
  # Run the function
  analyze_single_instance_data(instance_df, output_pdf_path)
  
  # Check that the PDF is created
  expect_true(file.exists(output_pdf_path))
})

test_that("gtable_add_padding1() returns a gtable object", {
  # Create a sample table
  example_table <- gridExtra::tableGrob(head(mtcars))
  
  # Add padding to the table
  padded_table <- gtable_add_padding1(example_table, grid::unit(c(1, 1, 1, 1), "cm"))
  
  # Check that the result is a gtable object
  expect_s3_class(padded_table, "gtable")
})
