# Hand-made data: one USD stock with a 2:1 split, one dividend and a partial sale
dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
pos <- list(list(id = "abc", name = "ABC", ticker = "ABC", type = "stock", currency = "USD",
                 withholding = 0.25,
                 lots = list(list(date = "2024-02-01", shares = 10, price = 100, fees = 2),
                             list(date = "2024-05-01", shares = 10, price = 120, fees = 2),
                             list(date = "2024-09-02", shares = -10, price = 70, fees = 2))))
pdata <- data.table::rbindlist(list(
  data.table::data.table(date = dates, id = "px_abc", value = 60),          # split-adjusted close
  data.table::data.table(date = as.Date("2024-07-01"), id = "spl_abc", value = 0.5),  # 2:1
  data.table::data.table(date = as.Date("2024-08-01"), id = "div_abc", value = 1),
  data.table::data.table(date = dates, id = "fx_USD", value = 1.25)))      # 1.25 USD per EUR

test_that("adjust_lots converts lots before a split to the adjusted basis", {
  l <- adjust_lots(pos[[1]], pdata[id == "spl_abc"])
  expect_equal(l$shares, c(20, 20, -10))
  expect_equal(l$price, c(50, 60, 70))
})

test_that("dividends count the shares held the day before the ex-date, net of withholding", {
  d <- dividends_received(pos, pdata)
  expect_equal(d$titulos, 40)
  expect_equal(d$bruto_eur, 40 * 1 / 1.25)
  expect_equal(d$neto_eur, 32 * 0.75)
})

test_that("position history and summary add up", {
  h <- position_history(pos, pdata)
  last <- h[.N]
  expect_equal(last$titulos, 30)
  expect_equal(last$valor_eur, 30 * 60 / 1.25)
  expect_equal(last$invertido_eur, (10 * 100 + 10 * 120) / 1.25 + 4)
  expect_equal(last$ventas_eur, 10 * 70 / 1.25 - 2)
  expect_equal(last$dividendos_eur, 24)
  s <- position_summary(pos, pdata, h)[id == "abc"]
  # average cost: 1764 EUR for 40 shares, 10 sold
  expect_equal(s$coste_eur, 1764 * 30 / 40)
  expect_equal(s$plusvalia_realizada_eur, (560 - 2) - 1764 / 4)
  expect_equal(s$plusvalia_latente_eur + s$plusvalia_realizada_eur + s$dividendos_eur,
               s$resultado_eur)
})

test_that("history uses a common calendar across exchanges", {
  p2 <- c(pos, list(list(id = "eur", name = "EUR stock", ticker = "E", type = "stock",
                         currency = "EUR", lots = list(list(date = "2024-02-01", shares = 1,
                                                            price = 10)))))
  d2 <- rbind(pdata, data.table::data.table(date = dates[dates != as.Date("2024-03-01")],
                                            id = "px_eur", value = 10))
  h <- position_history(p2, d2)
  expect_equal(h[date == as.Date("2024-03-01"), sort(id)], c("abc", "eur"))
})

test_that("year-to-date change uses the price history before the purchase", {
  p3 <- list(list(id = "new", name = "New", ticker = "N", type = "etf", currency = "EUR",
                  lots = list(list(date = "2024-06-03", shares = 1, price = 120))))
  d3 <- data.table::data.table(date = seq(as.Date("2023-12-01"), as.Date("2024-12-31"), by = "day"),
                               id = "px_new")
  d3[, value := data.table::fifelse(date < as.Date("2024-01-01"), 100, 125)]
  s <- position_summary(p3, d3, position_history(p3, d3))[id == "new"]
  expect_equal(s$var_anio_pct, 25)
})
