test_that("label_regimes splits inflation x credit with the configured thresholds", {
  m <- zoo::as.yearmon(2000 + 0:47 / 12)
  # CPI flat for 2 years, then +0.5 % a month (6.2 % yoy); credit tense in the last 6 months
  cpi <- 100 * cumprod(c(rep(1, 24), rep(1.005, 24)))
  baa <- c(rep(2, 42), rep(4, 6))
  w <- label_regimes(data.table::data.table(m_y = m, us_cpi = cpi, baa10y = baa), cfg)
  expect_true(all(is.na(w$regime[1:12])))                   # no yoy inflation yet
  expect_equal(w$regime[24], "Inflación baja · Crédito relajado")
  expect_equal(w$regime[40], "Inflación alta · Crédito relajado")
  expect_equal(w$regime[48], "Inflación alta · Crédito tensionado")
})

test_that("label_regimes interpolates gaps inside the inflation series only", {
  m <- zoo::as.yearmon(2000 + 0:35 / 12)
  cpi <- 100 * 1.002^(0:35)
  cpi[30] <- NA; cpi[36] <- NA
  w <- label_regimes(data.table::data.table(m_y = m, us_cpi = cpi, baa10y = 2), cfg)
  expect_false(is.na(w$regime[30]))
  expect_true(is.na(w$regime[36]))                           # trailing month not released yet
})

test_that("inflation regime has hysteresis between the entry and exit levels", {
  yoy <- c(2, 3.2, 2.8, 2.6, 2.4, 2.8, 3.1)                    # enters at 3.0, leaves below 2.5
  cpi <- 100 * c(rep(1, 36), 1 + yoy / 100)                 # 36 flat months: credit median needs 24
  m <- zoo::as.yearmon(2000 + seq_along(cpi) / 12)
  c2 <- cfg; c2$regimes$inflation_high <- 3; c2$regimes$inflation_exit <- 2.5
  w <- label_regimes(data.table::data.table(m_y = m, us_cpi = cpi, baa10y = 2, ea_hicp = cpi), c2)
  alta <- grepl("alta", utils::tail(w$regime, 7))
  expect_equal(alta, c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE))
  expect_equal(utils::tail(w$infl_yoy_info, 7), yoy)
  c2$regimes$inflation_exit <- NULL                           # no exit level: one threshold
  w <- label_regimes(data.table::data.table(m_y = m, us_cpi = cpi, baa10y = 2), c2)
  expect_equal(grepl("alta", utils::tail(w$regime, 7)), yoy > 3)
})
