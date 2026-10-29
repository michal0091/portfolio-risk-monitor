test_that("auc and brier match known values", {
  expect_equal(auc(c(.1, .2, .8, .9), c(0, 0, 1, 1)), 1)
  expect_equal(auc(c(.9, .8, .2, .1), c(0, 0, 1, 1)), 0)
  expect_equal(auc(c(.1, .4, .35, .8), c(0, 0, 1, 1)), 0.75)
  expect_equal(brier(c(1, 0, .5), c(1, 0, 1)), .25 / 3)
})

test_that("regime bootstrap is reproducible and exact with constant returns", {
  m <- zoo::as.yearmon(2000 + 0:119 / 12)
  pr <- data.table::data.table(m_y = m, regime = rep(c("A", "B"), each = 60),
                               `Cartera objetivo` = 0.01, `Ahorro general` = rnorm(120, 0, .05))
  c2 <- cfg; c2$forecast$bootstrap$n_paths <- 500; c2$forecast$bootstrap$min_regime_months <- 12
  b1 <- regime_bootstrap(pr, c2); b2 <- regime_bootstrap(pr, c2)
  expect_identical(b1$summary, b2$summary)
  expect_equal(b1$regime, "B")
  s <- b1$summary[cartera == "Cartera objetivo" & horizonte_meses == 24]
  expect_equal(s$rentab_p50, 100 * (1.01^24 - 1))
  expect_equal(s$prob_caida, 0)
  # too few months in the current regime -> all months are used
  c2$forecast$bootstrap$min_regime_months <- 100
  expect_equal(regime_bootstrap(pr, c2)$regime, "todos los regímenes")
})

test_that("logit target flags a drawdown beyond the threshold within the horizon", {
  dates <- seq(as.Date("2010-01-01"), as.Date("2012-12-31"), by = "day")
  px <- rep(100, length(dates)); px[dates >= as.Date("2011-06-01") & dates < as.Date("2011-07-01")] <- 80
  daily <- data.table::data.table(date = dates, id = "spx", value = px)
  w <- data.table::data.table(m_y = zoo::as.yearmon(2010 + 0:35 / 12), baa10y = 2, ust10 = 3,
                              ust2 = 2, tips10 = 1, vix = 15, sox = 100)
  d <- logit_data(daily, w, cfg)
  y <- d[, stats::setNames(y, format(m_y, "%Y-%m"))]
  expect_equal(unname(y[c("2010-11", "2010-12", "2011-05", "2011-06", "2011-07")]), c(0, 1, 1, 0, 0))
  expect_true(all(is.na(y[c("2012-07", "2012-12")])))       # future not observed yet
})

test_that("capm recovers a known beta", {
  set.seed(3)
  dates <- seq(as.Date("2018-01-01"), as.Date("2023-12-31"), by = "month")
  rm <- rnorm(length(dates), .005, .04)
  world <- 100 * cumprod(1 + rm)
  daily <- data.table::rbindlist(list(
    data.table::data.table(date = dates, id = "world", value = world),
    data.table::data.table(date = dates, id = "europe", value = 100 * cumprod(1 + 1.5 * rm)),
    data.table::data.table(date = dates, id = "em", value = 100 * cumprod(1 + rnorm(length(dates), 0, .03))),
    data.table::data.table(date = dates, id = "estr", value = 0)))
  ct <- capm_table(daily, NULL, cfg)
  expect_equal(ct[id == "europe", beta], 1.5, tolerance = 1e-8)
  expect_equal(ct[id == "europe", r2], 1)
  expect_equal(ct$meses, c(36, 36))
})

test_that("garch needs enough observations and reports positive tail losses", {
  expect_null(garch_one(100 * cumprod(1 + rnorm(100, 0, .01)), "corta", cfg))
  set.seed(11)
  g <- garch_one(100 * exp(cumsum(rnorm(1500, 0, .01))), "larga", cfg)
  expect_true(g$table$var99_1d_pct > g$table$var95_1d_pct)
  expect_true(g$table$es95_1d_pct > g$table$var95_1d_pct)
  expect_equal(g$table$vol_anual_pct, 100 * .01 * sqrt(252), tolerance = .25)
})

test_that("positions_index holds today's quantities over the whole price history", {
  dates <- as.Date("2024-01-01") + 0:4
  prices <- data.table::rbindlist(list(
    data.table::data.table(date = dates, id = "a", nombre = "A", unidad_eur = c(10, 11, 12, 13, 14)),
    data.table::data.table(date = dates[-3], id = "b", nombre = "B", unidad_eur = c(5, 5, 6, 6))))
  ps <- data.table::data.table(id = c("a", "b", "total"), titulos = c(2, 10, NA))
  idx <- positions_index(prices, ps)
  expect_equal(idx$valor, c(2 * 10 + 50, 2 * 11 + 50, 2 * 12 + 50, 2 * 13 + 60, 2 * 14 + 60))  # locf
})

test_that("risk contributions add up to 100 and favour the volatile position", {
  set.seed(5)
  dates <- as.Date("2024-01-01") + 0:299
  prices <- data.table::rbindlist(list(
    data.table::data.table(date = dates, id = "calm", nombre = "Calm", unidad_eur = 100 * exp(cumsum(rnorm(300, 0, .005)))),
    data.table::data.table(date = dates, id = "wild", nombre = "Wild", unidad_eur = 100 * exp(cumsum(rnorm(300, 0, .03))))))
  ps <- data.table::data.table(id = c("calm", "wild", "total"), nombre = c("Calm", "Wild", "Total"),
                               titulos = c(1, 1, NA), valor_eur = c(500, 500, 1000))
  rc <- risk_contribution(prices, ps, cfg)
  expect_equal(sum(rc$aporte_riesgo_pct), 100)
  expect_equal(rc$id[1], "wild")
  expect_equal(rc$peso_pct, c(50, 50))
})

test_that("garch_one reports half-life and last-week volatility", {
  set.seed(11)
  g <- garch_one(100 * exp(cumsum(rnorm(1500, 0, .01))), "larga", cfg)$table
  expect_true(g$media_vida_dias > 0)
  expect_true(is.finite(g$vol_hace_1s_pct))
})

test_that("garch long-run volatility is close to the simulated one", {
  set.seed(21)
  g <- garch_one(100 * exp(cumsum(rnorm(2500, 0, .01))), "x", cfg)$table
  if (is.finite(g$vol_largo_plazo_pct)) expect_equal(g$vol_largo_plazo_pct, 100 * .01 * sqrt(252), tolerance = .3)
  expect_true(g$vol_largo_plazo_pct > 0 || is.na(g$vol_largo_plazo_pct))
})
