test_that("scenario probabilities sum to one", {
  expect_equal(sum(unlist(cfg$scenarios$probabilities)), 1)
})

test_that("scenario_table returns the sleeve return when every sleeve returns the same", {
  c2 <- cfg
  c2$scenarios$returns <- lapply(cfg$scenarios$returns, function(r) lapply(r, function(x) -10))
  sc <- scenario_table(c2, pf)
  expect_equal(sc$scenario, c(names(cfg$scenarios$probabilities), "Esperado"))
  num <- c("fondos_actual", "fondos_objetivo", "plan_pensiones", "conjunto_objetivo")
  expect_true(all(abs(as.matrix(sc[, ..num]) + 10) < 1e-9))
})

test_that("portfolio_history tracks a single asset exactly whatever the rebalancing", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day")
  px <- 100 * (1 + seq_along(dates) / 1000)
  daily <- data.table::rbindlist(list(
    data.table::data.table(date = dates, id = "world", value = px),
    data.table::data.table(date = dates, id = "europe", value = 50),
    data.table::data.table(date = dates, id = "em", value = 50),
    data.table::data.table(date = dates, id = "estr", value = 0)))
  p1 <- pf; p1$weights_current <- list(groupama = 0, world = 100, europe = 0, em = 0)
  for (k in c(1, 3, 12)) {
    ph <- portfolio_history(daily, p1, every_months = k)[cartera == "Cartera actual"]
    expect_equal(ph$valor, 100 * px / px[1])
  }
})
