sig_with <- function(amber = character(), red = character()) {
  ids <- sapply(cfg$signals, `[[`, "id")
  s <- data.table::data.table(id = ids, senal = sapply(cfg$signals, `[[`, "label"), estado = "Verde")
  s[id %in% amber, estado := "Ámbar"][id %in% red, estado := "Rojo"]
  s
}

test_that("share-price drawdowns do not count as credit signals", {
  h <- scenario_hint(sig_with(amber = c("bizd_dd", "crwv_dd", "ust10", "tips10")))
  expect_equal(h$senales_credito, 0)
  expect_match(h$escenario_probable, "^A con tipos altos")
  expect_match(h$motivo, "CoreWeave")
  expect_match(h$motivo, "\\(ámbar\\)")
})

test_that("scenario B needs a tense spread and rate pressure", {
  expect_match(scenario_hint(sig_with(amber = c("hy_oas", "ust10", "brent")))$escenario_probable, "^B ")
  expect_match(scenario_hint(sig_with(amber = "hy_oas"))$escenario_probable, "^A \\(")
  expect_match(scenario_hint(sig_with(red = c("vix", "stlfsi")))$escenario_probable, "^C ")
  h <- scenario_hint(sig_with())
  expect_match(h$escenario_probable, "^D ")
  expect_equal(h$motivo, "todas las señales en verde")
})
