test_that("visualize_node() returns a ggplot object", {
  result_one <- visualize_node()
  expect_s3_class(result_one, "gg")  # Check that the result is a ggplot object
})

test_that("visualize_node_seasonal() returns a ggplot object", {
  result_two <- visualize_node_seasonal()
  expect_s3_class(result_two, "gg")  # Check that the result is a ggplot object
})

test_that("visualize_node_congestion() returns a ggplot object", {
  result_three <- visualize_node_congestion()
  expect_s3_class(result_three, "gg")  # Check that the result is a ggplot object
})

test_that("visualize_node_seasonal_congestion() returns a ggplot object", {
  result_four <- visualize_node_seasonal_congestion()
  expect_s3_class(result_four, "gg")  # Check that the result is a ggplot object
})

test_that("visualize_node_losses() returns a ggplot object", {
  result_five <- visualize_node_losses()
  expect_s3_class(result_five, "gg")  # Check that the result is a ggplot object
})

test_that("visualize_node_seasonal_losses() returns a ggplot object", {
  result_six <- visualize_node_seasonal_losses()
  expect_s3_class(result_six, "gg")  # Check that the result is a ggplot object
})

test_that("visualize_node_demand() returns a ggplot object", {
  result_seven <- visualize_node_demand()
  expect_s3_class(result_seven, "gg")  # Check that the result is a ggplot object
})

test_that("visualize_node_demand_without_high_demand_hours() returns a ggplot object", {
  result_eight <- visualize_node_demand_without_high_demand_hours()
  expect_s3_class(result_eight, "gg")  # Check that the result is a ggplot object
})

test_that("visualize_node_demand_congestion() returns a ggplot object", {
  result_nine <- visualize_node_demand_congestion()
  expect_s3_class(result_nine, "gg")  # Check that the result is a ggplot object
})

test_that("visualize_node_demand_without_high_demand_hours_congestion() returns a ggplot object", {
  result_ten <- visualize_node_demand_without_high_demand_hours_congestion()
  expect_s3_class(result_ten, "gg")  # Check that the result is a ggplot object
})

