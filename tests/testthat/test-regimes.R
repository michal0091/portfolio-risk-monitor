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
