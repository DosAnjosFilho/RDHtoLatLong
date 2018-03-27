test_that("Test with the coordinates of the city of Amsterdam", {
  expect_equal(RDHtoLatLong(121687, 487484), c(52.374216, 4.898012))
})
