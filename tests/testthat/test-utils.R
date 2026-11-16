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

test_that("append_recent only adds dates after the history", {
  h <- data.table::data.table(date = as.Date("2026-11-10") + 0:1, id = "x", value = c(90, 92))
  r <- data.table::data.table(date = as.Date("2026-11-10") + 0:3, id = "x", value = c(1, 2, 95, NA))
  res <- append_recent(h, r)
  expect_equal(res$value, c(90, 92, 95))
  expect_equal(append_recent(h[0], r)$value, c(1, 2, 95))
})

test_that("validate_daily removes bad ticks and only reports large moves that stay", {
  d <- as.Date("2026-11-01") + 0:6
  daily <- data.table::rbindlist(list(
    data.table::data.table(date = d, id = "tick", value = c(100, 101, 10, 102, 103, 104, 105)),
    data.table::data.table(date = d, id = "crash", value = c(100, 100, 100, 70, 71, 70, 69)),
    data.table::data.table(date = d, id = "zero", value = c(5, 5, 0, 5, 5, 5, 5)),
    data.table::data.table(date = d, id = "rate", value = c(1, -1, 3, 0, 1, 1, 1))))   # not a price
  attr(daily, "stale") <- "x"
  v <- validate_daily(daily, c("tick", "crash", "zero"))
  expect_equal(v$issues[excluido == TRUE, id], c("tick", "zero"))
  expect_equal(v$issues[excluido == FALSE, .(id, date)], data.table::data.table(id = "crash", date = d[4]))
  expect_equal(nrow(v$daily), nrow(daily) - 2)
  expect_equal(nrow(v$daily[id == "crash"]), 7)                 # a real fall is kept
  expect_equal(nrow(v$daily[id == "rate"]), 7)
  expect_equal(attr(v$daily, "stale"), "x")
  expect_equal(nrow(validate_daily(daily[id == "rate"], "tick")$issues), 0)
})

test_that("freshness_table flags a series that stopped updating for its frequency", {
  today <- as.Date("2026-11-16")
  daily <- data.table::rbindlist(list(
    data.table::data.table(date = today - 3:40, id = "ok_daily", value = 1),
    data.table::data.table(date = today - 10:50, id = "late_daily", value = 1),
    data.table::data.table(date = today - 7 * (2:20), id = "ok_weekly", value = 1),
    data.table::data.table(date = seq(as.Date("2025-01-01"), as.Date("2026-09-01"), by = "month"), id = "ok_monthly", value = 1)))
  f <- freshness_table(daily, today = today)
  expect_equal(f[desfasada == TRUE, id], "late_daily")
  expect_equal(f[id == "ok_weekly", frecuencia], "semanal")
  expect_equal(f[id == "ok_monthly", frecuencia], "mensual")
  expect_false(freshness_table(daily, today = today, max_lag = c(late_daily = 12))[id == "late_daily", desfasada])
  expect_equal(nrow(freshness_table(daily, "ok_daily", today)), 1)
})
