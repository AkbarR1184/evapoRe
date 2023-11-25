test_that("evapoRe works", {
  dates <- seq(as.Date("2023-01-01"), by = "month", length.out = 12)
  values <- array(1:120, dim = c(10, 10, 12))
  x <- raster::brick(values)
  x <- raster::setZ(x, dates)
  original_dims <- dim(x)
  result <- muldpm(x)
  expect_type(result, "S4")
  expect_equal(original_dims, dim(result),
               info = "Dimensions should remain the same before and after applying muldpm")
})