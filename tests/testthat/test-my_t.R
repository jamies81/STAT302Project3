test_that("my_t.test works", {
  expect_type(my_t.test(my_penguins$body_mass_g, alternative = "two.sided", mu = 4200),
               "list")
})
