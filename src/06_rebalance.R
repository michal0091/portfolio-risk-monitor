# Rebalancing calculator: bands check + traspasos to target ------------------------
# Works in % of the portfolio; if values_eur are filled in cfg/portfolio.yaml it also
# returns euro amounts. Traspasos between funds are tax-free in Spain: always transfer,
# never redeem and subscribe.

rebalance_plan <- function(pf, tranches = pf$rebalance$tranches) {
  ids <- names(pf$weights_target)
  vals <- unlist(lapply(pf$values_eur[ids], function(v) if (is.null(v)) NA_real_ else v))
  has_eur <- all(!is.na(vals))
  cur <- if (has_eur) 100 * vals / sum(vals) else unlist(pf$weights_current)[ids]
  tgt <- unlist(pf$weights_target)[ids]
  band <- pf$rebalance$band_pp
  dt <- data.table::data.table(
    fondo = ids, nombre = sapply(pf$funds[ids], `[[`, "name"),
    peso_actual = cur, peso_objetivo = tgt, desvio_pp = cur - tgt)
  dt[, fuera_de_banda := abs(desvio_pp) > band]
  dt[, traspaso_pp := -desvio_pp]
  dt[, traspaso_pp_por_tramo := traspaso_pp / tranches]
  if (has_eur) {
    total <- sum(vals)
    dt[, traspaso_eur := traspaso_pp / 100 * total]
    dt[, traspaso_eur_por_tramo := traspaso_eur / tranches]
  }
  dt[]
}

# Glide path of the house-deposit bucket: equity share by year
glide_path <- function(pf, years = 0:6) {
  start <- pf$buckets$house_deposit$equity_share
  step  <- pf$rebalance$glide_pp_per_year
  data.table::data.table(anio = years, bolsa_bolsillo_entrada = pmax(0, start - step * years))
}

# Contribution router: which fund gets this month's equity contribution
route_contribution <- function(pf, equity_eur = 100) {
  ids <- c("world", "europe", "em")
  vals <- unlist(lapply(pf$values_eur[ids], function(v) if (is.null(v)) NA_real_ else v))
  cur <- if (all(!is.na(vals))) 100 * vals / sum(unlist(pf$values_eur), na.rm = TRUE)
         else unlist(pf$weights_current)[ids]
  gap <- unlist(pf$weights_target)[ids] - cur
  data.table::data.table(fondo = ids, hueco_pp = gap,
                         aportacion_eur = ifelse(ids == ids[which.max(gap)], equity_eur, 0))
}
