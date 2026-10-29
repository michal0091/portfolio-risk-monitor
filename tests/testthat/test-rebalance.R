test_that("example weights are consistent", {
  expect_equal(sum(unlist(pf$weights_target)), 100)
  expect_equal(sum(unlist(pf$weights_current)), 100)
  buckets <- sum(sapply(pf$buckets, `[[`, "groupama"))
  expect_equal(buckets, pf$weights_target$groupama)
})

test_that("rebalance_plan transfers net to zero and land on target", {
  reb <- rebalance_plan(pf)
  expect_equal(sum(reb$traspaso_pp), 0)
  expect_equal(reb$peso_actual + reb$traspaso_pp, reb$peso_objetivo)
  expect_equal(reb$traspaso_pp_por_tramo * pf$rebalance$tranches, reb$traspaso_pp)
})

test_that("rebalance_plan works in euros when values_eur are given", {
  p2 <- pf
  p2$values_eur <- list(groupama = 3500, world = 3000, europe = 2000, em = 1500)
  reb <- rebalance_plan(p2)
  expect_equal(sum(reb$traspaso_eur), 0)
  expect_equal(reb$traspaso_eur[reb$fondo == "groupama"], 1500)
})

test_that("fund_drift moves weights with the proxies since as_of", {
  dates <- as.Date(c("2026-01-01", "2026-12-31"))
  daily <- data.table::rbindlist(list(
    data.table::data.table(date = dates, id = "world", value = c(100, 200)),
    data.table::data.table(date = dates, id = "europe", value = 100),
    data.table::data.table(date = dates, id = "em", value = 100),
    data.table::data.table(date = dates, id = "estr", value = 0)))
  p2 <- pf; p2$as_of <- "2026-01-01"
  d <- fund_drift(daily, p2)
  w0 <- unlist(pf$weights_current)
  expect_equal(d$peso_estimado, unname(100 * w0 * c(1, 2, 1, 1) / sum(w0 * c(1, 2, 1, 1))))
  expect_equal(sum(d$peso_estimado), 100)
  p2$as_of <- NULL
  expect_equal(fund_drift(daily, p2)$peso_estimado, unname(w0))
})
