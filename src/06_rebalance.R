# Rebalancing calculator: bands check + traspasos to target ------------------------
# Works in % of the portfolio; if values_eur are filled in cfg/portfolio.yaml it also
# returns euro amounts. Traspasos between funds are tax-free in Spain: always transfer,
# never redeem and subscribe.

# State of the tranche plan in pf$rebalance_plan (start, tranches: [{date, done}]).
# Active while a tranche is not done; the deviation left is then split among the pending
# tranches only. Without a plan a new rebalance is split in pf$rebalance$tranches.
plan_status <- function(pf) {
  tr <- pf$rebalance_plan$tranches
  done <- vapply(tr, function(t) isTRUE(t$done), logical(1))
  if (!length(tr) || all(done)) {
    n <- if (is.null(pf$rebalance$tranches)) 1 else pf$rebalance$tranches
    return(list(active = FALSE, pending = n, total = n, next_n = 1, next_date = as.Date(NA)))
  }
  nxt <- which(!done)[1]
  list(active = TRUE, pending = sum(!done), total = length(tr), next_n = nxt,
       next_date = if (is.null(tr[[nxt]]$date)) as.Date(NA) else as.Date(tr[[nxt]]$date))
}

# Transfers of the next tranche: only funds outside their band or above the minimum
# amount (pf$rebalance$min_transfer_eur, or 0.5 pp when working in percentages), scaled
# so that what is given equals what is received. d: fund_drift() output.
tranche_transfers <- function(d, pf) {
  eur <- "traspaso_eur_por_tramo" %in% names(d)
  x <- d[, .(fondo, nombre, fuera_de_banda,
             importe = if (eur) traspaso_eur_por_tramo else -desvio_pp / plan_status(pf)$pending)]
  min_t <- if (!eur) 0.5 else if (is.null(pf$rebalance$min_transfer_eur)) 100 else pf$rebalance$min_transfer_eur
  x <- x[fuera_de_banda | abs(importe) >= min_t]
  inn <- x[importe > 0, sum(importe)]; out <- x[importe < 0, -sum(importe)]
  if (inn == 0 || out == 0) return(x[0, .(fondo, nombre, importe)])
  x[importe > 0, importe := importe * min(inn, out) / inn]
  x[importe < 0, importe := importe * min(inn, out) / out]
  x[, .(fondo, nombre, importe)]
}

rebalance_plan <- function(pf, tranches = plan_status(pf)$pending) {
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

# Current fund weights (and EUR values when values_eur is filled) estimated from the
# snapshot taken on pf$as_of, drifted with each fund's proxy (cash proxies such as estr
# accrue ACT/360). Without as_of the snapshot is taken as current. Also the value on the
# previous session, for the daily change, and the transfer to target per fund.
fund_drift <- function(daily, pf, rate_proxies = "estr") {
  ids <- names(pf$weights_target)
  w0 <- unlist(pf$weights_current)[ids]; tgt <- unlist(pf$weights_target)[ids]
  base <- if (isTRUE(pf$values_based)) unlist(pf$values_eur)[ids] else w0
  from <- if (is.null(pf$as_of)) NA else as.Date(pf$as_of)
  growth <- function(f, drop_last = FALSE) {
    if (is.na(from)) return(1)
    proxy <- pf$funds[[f]]$proxy
    x <- daily[id == proxy & date >= from & !is.na(value)][order(date)]
    if (drop_last) x <- x[-nrow(x)]
    if (nrow(x) < 2) return(1)
    if (proxy %in% rate_proxies) prod(1 + utils::head(x$value, -1) / 100 * diff(as.numeric(x$date)) / 360)
    else data.table::last(x$value) / x$value[1]
  }
  now  <- base * sapply(ids, growth)
  prev <- base * sapply(ids, growth, drop_last = TRUE)
  w <- 100 * now / sum(now)
  out <- data.table::data.table(fondo = ids, nombre = sapply(pf$funds[ids], `[[`, "name"),
                                peso_estimado = unname(w), peso_objetivo = unname(tgt),
                                desvio_pp = unname(w - tgt),
                                fuera_de_banda = unname(abs(w - tgt) > pf$rebalance$band_pp),
                                desde = from)
  if (isTRUE(pf$values_based)) {
    total <- sum(now)
    out[, `:=`(valor_estimado_eur = unname(now), valor_previo_eur = unname(prev),
               traspaso_eur = (peso_objetivo - peso_estimado) / 100 * total,
               traspaso_eur_por_tramo = (peso_objetivo - peso_estimado) / 100 * total /
                 plan_status(pf)$pending)]
  }
  out[]
}
