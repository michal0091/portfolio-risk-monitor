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
