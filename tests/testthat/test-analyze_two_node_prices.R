test_that("compare_lmps_visual returns a ggplot object with empty arguments", {
  result_one <- compare_lmps_visual()
  expect_s3_class(result_one, "gg")  # Check that the result is a ggplot object
})

test_that("compare_lmps_visual_seasonal returns a ggplot object with empty arguments", {
  result_two <- compare_lmps_visual_seasonal()
  expect_s3_class(result_two, "gg")  # Check that the result is a ggplot object
})

test_that("compare_congestion_visual returns a ggplot object with empty arguments", {
  result_three <- compare_congestion_visual()
  expect_s3_class(result_three, "gg")  # Check that the result is a ggplot object
})

test_that("compare_congestion_visual_seasonal returns a ggplot object with empty arguments", {
  result_four <- compare_congestion_visual_seasonal()
  expect_s3_class(result_four, "gg")  # Check that the result is a ggplot object
})

test_that("compare_losses_visual returns a ggplot object with empty arguments", {
  result_five <- compare_losses_visual()
  expect_s3_class(result_five, "gg")  # Check that the result is a ggplot object
})

test_that("compare_losses_visual_seasonal returns a ggplot object with empty arguments", {
  result_six <- compare_losses_visual_seasonal()
  expect_s3_class(result_six, "gg")  # Check that the result is a ggplot object
})


test_that("analyze_two_node_data() has a message when it is run", {
  expect_message(analyze_two_node_data())
})
