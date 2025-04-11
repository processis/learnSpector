test_that("add_two_numbers works correctly", {
  # Define the inputs and expected outputs
  input1 <- 2
  input2 <- 3
  expected_output1 <- 5

  # Test if the function returns the correct sum
  expect_equal(my_sum(input1, input2), expected_output1)

  # Test if the function throws an error for incorrect input
  wrong_input <- "a"
  expect_error(my_sum(wrong_input, input2))
})
