test_that("example weights are consistent", {
  expect_equal(sum(unlist(pf$weights_target)), 100)
  expect_equal(sum(unlist(pf$weights_current)), 100)
  buckets <- sum(sapply(pf$buckets, `[[`, "groupama"))
  expect_equal(buckets, pf$weights_target$groupama)
})

none <- data.table::data.table(date = as.Date(character()), id = character(), value = numeric())

test_that("without market data fund_drift gives the transfers of the snapshot", {
  d <- fund_drift(none, pf)
  expect_equal(sum(d$desvio_pp), 0)
  expect_equal(d$peso_estimado, unname(unlist(pf$weights_current)))
  p2 <- pf; p2$values_eur <- list(groupama = 3500, world = 3000, europe = 2000, em = 1500)
  d <- fund_drift(none, normalize_portfolio(p2))
  expect_equal(sum(d$traspaso_eur), 0)
  expect_equal(d[fondo == "groupama", traspaso_eur], 1500)
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

test_that("values_eur replaces weights_current and gives EUR estimates", {
  p2 <- pf; p2$values_eur <- list(groupama = 5000, world = 3000, europe = 1000, em = 1000)
  p2 <- normalize_portfolio(p2)
  expect_true(p2$values_based)
  expect_equal(unlist(p2$weights_current), c(groupama = 50, world = 30, europe = 10, em = 10))
  expect_false(normalize_portfolio(pf)$values_based)          # nulls -> percentages only
  dates <- as.Date(c("2026-01-01", "2026-06-30", "2026-07-01"))
  daily <- data.table::rbindlist(list(
    data.table::data.table(date = dates, id = "world", value = c(100, 110, 121)),
    data.table::data.table(date = dates, id = c("europe"), value = 100),
    data.table::data.table(date = dates, id = c("em"), value = 100),
    data.table::data.table(date = dates, id = "estr", value = 0)))
  p2$as_of <- "2026-01-01"
  d <- fund_drift(daily, p2)
  expect_equal(d[fondo == "world", valor_estimado_eur], 3000 * 1.21)
  expect_equal(d[fondo == "world", valor_previo_eur], 3000 * 1.10)
  total <- sum(d$valor_estimado_eur)
  expect_equal(sum(d$traspaso_eur), 0)
  expect_equal(d[fondo == "groupama", traspaso_eur], (pf$weights_target$groupama / 100) * total - 5000)
})

test_that("the next tranche is the pending deviation split among the tranches not done", {
  p2 <- pf; p2$values_eur <- list(groupama = 3500, world = 3000, europe = 2000, em = 1500)
  p2 <- normalize_portfolio(p2); p2$as_of <- NULL
  d0 <- fund_drift(none, p2)
  expect_equal(d0$traspaso_eur_por_tramo, d0$traspaso_eur / 2)   # no plan: rebalance$tranches
  p2$rebalance_plan <- list(start = "2026-10-27",
                            tranches = list(list(date = "2026-10-30", done = TRUE),
                                            list(date = "2026-12-15", done = FALSE)))
  ps <- plan_status(p2)
  expect_true(ps$active)
  expect_equal(c(ps$pending, ps$next_n, ps$total), c(1, 2, 2))
  expect_equal(ps$next_date, as.Date("2026-12-15"))
  d1 <- fund_drift(none, p2)
  expect_equal(d1$traspaso_eur_por_tramo, d1$traspaso_eur)        # one tranche left: all of it
  p2$rebalance_plan$tranches[[2]]$done <- TRUE
  expect_false(plan_status(p2)$active)                            # finished plan = no plan
})

test_that("tranche transfers skip small in-band amounts and net to zero", {
  d <- data.table::data.table(fondo = c("g", "w", "e", "m"), nombre = c("G", "W", "E", "M"),
                              desvio_pp = c(-7, 4, 0.3, 2.7), fuera_de_banda = c(TRUE, FALSE, FALSE, FALSE),
                              traspaso_eur_por_tramo = c(1041, -600, -42, -399))
  tr <- tranche_transfers(d, pf)
  expect_equal(tr$fondo, c("g", "w", "m"))                        # 42 EUR inside the band: left out
  expect_equal(sum(tr$importe), 0)
  expect_equal(tr[fondo == "g", importe], 999)                    # scaled to what the others give
  p2 <- pf; p2$rebalance$min_transfer_eur <- 500
  expect_equal(tranche_transfers(d, p2)$fondo, c("g", "w"))
  d[, traspaso_eur_por_tramo := NULL]                             # percentages only
  expect_equal(sum(tranche_transfers(d, pf)$importe), 0)
})

test_that("transactions are applied on their date over the snapshot", {
  dates <- as.Date(c("2026-01-01", "2026-03-02", "2026-06-30", "2026-07-01"))
  daily <- data.table::rbindlist(list(
    data.table::data.table(date = dates, id = "world", value = c(100, 110, 121, 121)),
    data.table::data.table(date = dates, id = "europe", value = 100),
    data.table::data.table(date = dates, id = "em", value = 100),
    data.table::data.table(date = dates, id = "estr", value = 0)))
  p2 <- pf; p2$values_eur <- list(groupama = 5000, world = 3000, europe = 1000, em = 1000)
  p2 <- normalize_portfolio(p2); p2$as_of <- "2026-01-01"
  p2$transactions <- list(list(date = "2026-03-02", from = "world", to = "groupama", eur = 1100),
                          list(date = "2026-07-01", to = "em", eur = 50),          # contribution
                          list(date = "2026-12-01", from = "europe", eur = 999),   # future: not yet
                          list(date = "2025-12-01", from = "europe", eur = 999))   # before the snapshot
  d <- fund_drift(daily, p2)
  expect_equal(d[fondo == "world", valor_estimado_eur], (3000 * 1.1 - 1100) * 1.1)
  expect_equal(d[fondo == "groupama", valor_estimado_eur], 6100)
  expect_equal(d[fondo == "em", c(valor_previo_eur, valor_estimado_eur)], c(1000, 1050))
  expect_equal(d[fondo == "europe", valor_estimado_eur], 1000)
  expect_equal(sum(d$traspaso_eur), 0)
})

test_that("cash proxies accrue ACT/360 with the last known rate", {
  daily <- data.table::rbindlist(list(
    data.table::data.table(date = as.Date(c("2026-01-01", "2026-12-27")), id = "world", value = 100),
    data.table::data.table(date = as.Date(c("2026-01-01", "2026-12-27")), id = "europe", value = 100),
    data.table::data.table(date = as.Date(c("2026-01-01", "2026-12-27")), id = "em", value = 100),
    data.table::data.table(date = as.Date("2026-01-01"), id = "estr", value = 3.6)))   # lagging series
  p2 <- pf; p2$values_eur <- list(groupama = 1000, world = 1000, europe = 1000, em = 1000)
  p2 <- normalize_portfolio(p2); p2$as_of <- "2026-01-01"
  expect_equal(fund_drift(daily, p2)[fondo == "groupama", valor_estimado_eur], 1000 * (1 + .036 * 360 / 360))
})
