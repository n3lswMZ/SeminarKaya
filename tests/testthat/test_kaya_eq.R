test_that("Kaya Identity outputs errors with wrong inputs", {
  # character instead of numeric input
  expect_error(kaya_eq("a", 10, 5, 6))
  expect_error(kaya_eq(2, "a", 5, 6))
  expect_error(kaya_eq(1, 2, "a", 3))
  expect_error(kaya_eq(1, 2, 5, "a"))
  # negativ instead of positive inputs
  expect_error(kaya_eq(-1, 2, 3, 4))
  expect_error(kaya_eq(1, -2, 3, 4))
  expect_error(kaya_eq(1, 2, -3, 4))
  expect_error(kaya_eq(1, 2, 3, -4))
  # missing arguments
  expect_error(kaya_eq())
  expect_error(kaya_eq(pop = 1, gdp = 2, enInt = 3))
  expect_error(kaya_eq(pop = 2, gdp = 2, carbInt = 3))
  expect_error(kaya_eq(pop = 2, enInt = 3, carbInt = 2))
  # NA instead of numeric input
  expect_error(kaya_eq(NA, 1, 2, 3))
  expect_error(kaya_eq(1, NA, 2, 3))
  expect_error(kaya_eq(1, 2, NA, 3))
  expect_error(kaya_eq(1, 2, 3, NA))
  # numeric instead of character input
  expect_error(kaya_eq(1, 2, 3, 4, outputType = 2))
  # NA instead of character input
  expect_error(kaya_eq(1, 2, 3, 4, outputType = NA))
  # wrong character input
  expect_error(kaya_eq(1, 2, 3, 4, outputType = "H2O"))
})

test_that("Kaya Identity correct output for (default) output type CO2", {
  germany <- kaya_eq(pop = 82.4, gdp = 44, enInt = 5, carbInt = 0.05)
  expect_equal(germany, 906.4)
  expect_true(is.numeric(germany))
  
  expect_equal(kaya_eq(pop = 0, gdp = 44, enInt = 5, carbInt = 0.05), 0)
  expect_equal(kaya_eq(pop = 82.4, gdp = 0, enInt = 5, carbInt = 0.05), 0)
  expect_equal(kaya_eq(pop = 82.4, gdp = 44, enInt = 0, carbInt = 0.05), 0)
  expect_equal(kaya_eq(pop = 82.4, gdp = 44, enInt = 5, carbInt = 0), 0)
})

test_that("Kaya Identity correct output for output type C", {
  test_case <- kaya_eq(pop = 1, gdp = 1, enInt = 1, carbInt = 1, outputType = "C")
  expect_equal(test_case, 1/3.67)
  expect_true(is.numeric(test_case))
  
  expect_equal(kaya_eq(pop = 0, gdp = 44, enInt = 5, carbInt = 0.05, outputType = "C"), 0)
  expect_equal(kaya_eq(pop = 82.4, gdp = 0, enInt = 5, carbInt = 0.05, outputType = "C"), 0)
  expect_equal(kaya_eq(pop = 82.4, gdp = 44, enInt = 0, carbInt = 0.05, outputType = "C"), 0)
  expect_equal(kaya_eq(pop = 82.4, gdp = 44, enInt = 5, carbInt = 0, outputType = "C"), 0)
})