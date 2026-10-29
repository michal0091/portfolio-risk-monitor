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

# Current fund weights estimated from weights_current (as of pf$as_of) drifted with each
# fund's proxy (cash proxies such as estr accrue ACT/360). Without as_of the configured
# weights are taken as current.
fund_drift <- function(daily, pf, rate_proxies = "estr") {
  ids <- names(pf$weights_target)
  w0 <- unlist(pf$weights_current)[ids]; tgt <- unlist(pf$weights_target)[ids]
  growth <- rep(1, length(ids))
  from <- if (is.null(pf$as_of)) NA else as.Date(pf$as_of)
  if (!is.na(from)) {
    growth <- sapply(ids, function(f) {
      proxy <- pf$funds[[f]]$proxy
      x <- daily[id == proxy & date >= from & !is.na(value)][order(date)]
      if (nrow(x) < 2) return(1)
      if (proxy %in% rate_proxies) prod(1 + utils::head(x$value, -1) / 100 * diff(as.numeric(x$date)) / 360)
      else data.table::last(x$value) / x$value[1]
    })
  }
  w <- 100 * w0 * growth / sum(w0 * growth)
  data.table::data.table(fondo = ids, nombre = sapply(pf$funds[ids], `[[`, "name"),
                         peso_estimado = w, peso_objetivo = tgt, desvio_pp = w - tgt,
                         fuera_de_banda = abs(w - tgt) > pf$rebalance$band_pp, desde = from)
}
