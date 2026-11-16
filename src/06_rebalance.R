# Rebalancing: bands check + traspasos to target (fund_drift) ------------------------
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

# Glide path of the house-deposit bucket: equity share by year
glide_path <- function(pf, years = 0:6) {
  start <- pf$buckets$house_deposit$equity_share
  step  <- pf$rebalance$glide_pp_per_year
  data.table::data.table(anio = years, bolsa_bolsillo_entrada = pmax(0, start - step * years))
}

# Current fund weights (and EUR values when values_eur is filled) estimated from the
# snapshot taken on pf$as_of, drifted with each fund's proxy (cash proxies such as estr
# accrue ACT/360). Without as_of the snapshot is taken as current. Also the value on the
# previous session, for the daily change, and the transfer to target per fund.
# pf$transactions ([{date, from, to, eur}], after as_of, EUR snapshot only) are applied on
# their date: a transfer between funds, a contribution (no `from`) or a withdrawal (no `to`).
fund_drift <- function(daily, pf, rate_proxies = "estr") {
  ids <- names(pf$weights_target)
  w0 <- unlist(pf$weights_current)[ids]; tgt <- unlist(pf$weights_target)[ids]
  base <- if (isTRUE(pf$values_based)) unlist(pf$values_eur)[ids] else w0
  from <- if (is.null(pf$as_of)) NA else as.Date(pf$as_of)
  proxy <- sapply(pf$funds[ids], `[[`, "proxy")
  px <- daily[id %in% proxy & !is.na(value)][order(id, date)]
  # value of a proxy on a date: last observation up to it (the first one if none yet)
  at <- function(p, d) {
    x <- px[id == p]
    if (!nrow(x)) return(NA_real_)
    i <- findInterval(as.numeric(d), as.numeric(x$date))
    x$value[pmax(i, 1)]
  }
  growth <- function(a, b) sapply(ids, function(f) {
    p <- proxy[[f]]
    if (b <= a || !nrow(px[id == p])) return(1)
    if (p %in% rate_proxies) {
      grid <- sort(unique(c(a, px[id == p & date > a & date < b, date], b)))
      prod(1 + at(p, utils::head(grid, -1)) / 100 * diff(as.numeric(grid)) / 360)
    } else at(p, b) / at(p, a)
  })
  tx <- pf$transactions
  if (length(tx) && (is.na(from) || !isTRUE(pf$values_based))) {
    log_msg("WARN transactions need values_eur and as_of: ignored"); tx <- NULL
  }
  if (length(tx)) tx <- tx[order(as.Date(sapply(tx, `[[`, "date")))]
  value_at <- function(d) {
    v <- base; t0 <- from
    for (t in tx) {
      td <- as.Date(t$date)
      if (td <= from || td > d) next
      v <- v * growth(t0, td); t0 <- td
      if (!is.null(t$from)) v[t$from] <- v[t$from] - t$eur
      if (!is.null(t$to))   v[t$to]   <- v[t$to] + t$eur
    }
    v * growth(t0, d)
  }
  days <- if (is.na(from)) as.Date(character()) else sort(unique(px[date >= from, date]))
  now  <- if (length(days)) value_at(max(days)) else base
  prev <- if (length(days) > 1) value_at(days[length(days) - 1]) else now
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
