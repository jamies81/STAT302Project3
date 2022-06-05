#update this with correct input and output
test_that("my lm works", {
  expect_type(my_lm(my_penguins$bill_length_mm, my_penguins$bill_depth_mm),
               "list")
})
