test_that("drawdown_pct measures the loss from the running peak", {
  expect_equal(drawdown_pct(c(100, 120, 90, 130)), c(0, 0, 25, 0))
  expect_equal(drawdown_pct(c(NA, 100, 50))[2:3], c(0, 50))
})

test_that("merge_cache keeps old dates and lets the fresh download win", {
  f <- tempfile(fileext = ".csv")
  data.table::fwrite(data.table::data.table(
    date = as.Date(c("2020-01-01", "2020-01-02")), id = "x", value = c(1, 2)), f)
  new <- data.table::data.table(date = as.Date(c("2020-01-02", "2020-01-03")), id = "x",
                                value = c(9, 3))
  res <- merge_cache(new, f)
  expect_equal(res$date, as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")))
  expect_equal(res$value, c(1, 9, 3))
})
