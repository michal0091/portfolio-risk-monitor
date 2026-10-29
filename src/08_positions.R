# Tracked positions (stocks, ETFs, funds) bought on given dates -----------------------
# Data ids in the long table: px_<id> close (split-adjusted, NOT dividend-adjusted),
# div_<id> dividend per share (split-adjusted), spl_<id> split ratio, fx_<CUR> CUR per EUR.
# Yahoo adjusts closes and dividends for splits, so lots are converted to the same basis.

empty_series <- function(id) {
  data.table::data.table(date = as.Date(character()), id = character(), value = numeric())
}

xts_or_empty <- function(x, id) {
  if (is.null(x) || !length(x) || all(is.na(x))) return(empty_series(id))
  xts_to_dt(x, id)[!is.na(value)]
}

# Pence-quoted London tickers ("GBp") are converted to GBP
quote_ccy  <- function(ccy) if (identical(ccy, "GBp")) "GBP" else ccy
quote_unit <- function(ccy) if (identical(ccy, "GBp")) 100 else 1

download_positions <- function(positions, cfg) {
  from <- min(as.Date(unlist(lapply(positions, function(p) sapply(p$lots, `[[`, "date"))))) - 10
  y <- function(f, ...) f(..., src = "yahoo", from = from, auto.assign = FALSE)
  jobs <- unlist(lapply(positions, function(p) list(
    list(id = paste0("px_", p$id),
         f = function() xts_or_empty(quantmod::Cl(y(quantmod::getSymbols, p$ticker,
                                                     warnings = FALSE)), paste0("px_", p$id))),
    list(id = paste0("div_", p$id),
         f = function() xts_or_empty(y(quantmod::getDividends, p$ticker), paste0("div_", p$id))),
    list(id = paste0("spl_", p$id),
         f = function() xts_or_empty(y(quantmod::getSplits, p$ticker), paste0("spl_", p$id)))
  )), recursive = FALSE)
  ccys <- setdiff(unique(sapply(positions, function(p) quote_ccy(p$currency))), "EUR")
  fx_jobs <- lapply(ccys, function(cc) list(
    id = paste0("fx_", cc),
    f = function() xts_or_empty(quantmod::Cl(y(quantmod::getSymbols, paste0("EUR", cc, "=X"),
                                              warnings = FALSE)), paste0("fx_", cc))))
  run_fetch_jobs(c(jobs, fx_jobs), cfg, private = TRUE)
}

# Lots in the split-adjusted basis of Yahoo (ratio 0.1 = 10 new shares per old share)
adjust_lots <- function(p, splits) {
  lots <- data.table::rbindlist(lapply(p$lots, function(l) data.table::data.table(
    date = as.Date(l$date), shares = as.numeric(l$shares), price = as.numeric(l$price),
    fees = if (is.null(l$fees)) 0 else as.numeric(l$fees))))
  lots[, factor := sapply(date, function(d) prod(splits[date > d, value]))]
  lots[, `:=`(shares = shares / factor, price = price * factor)][, factor := NULL]
  lots[order(date)]
}

# EUR per unit of the position currency at given dates (last available rate)
fx_at <- function(pdata, ccy, dates) {
  cc <- quote_ccy(ccy)
  if (cc == "EUR") return(rep(1 / quote_unit(ccy), length(dates)))
  fx <- pdata[id == paste0("fx_", cc), .(date, rate = value)]
  if (!nrow(fx)) stop("no FX data for ", cc)
  r <- fx[data.table::data.table(date = dates), on = "date", roll = "nearest", rate]
  1 / (r * quote_unit(ccy))
}

# Dividends cashed per position: shares held the day before the ex-date
dividends_received <- function(positions, pdata) {
  data.table::rbindlist(lapply(positions, function(p) {
    d <- pdata[id == paste0("div_", p$id), .(fecha_ex = date, dps = value)]
    if (!nrow(d)) return(NULL)
    lots <- adjust_lots(p, pdata[id == paste0("spl_", p$id)])
    d[, titulos := sapply(fecha_ex, function(x) sum(lots[date < x, shares]))]
    d <- d[titulos > 0]
    if (!nrow(d)) return(NULL)
    wh <- if (is.null(p$withholding)) 0 else p$withholding
    d[, bruto_eur := titulos * dps * fx_at(pdata, p$currency, fecha_ex)]
    d[, `:=`(id = p$id, nombre = p$name, neto_eur = bruto_eur * (1 - wh))]
    data.table::setcolorder(d, c("id", "nombre"))
  }), use.names = TRUE)
}

# Daily value and result of each position in EUR
position_history <- function(positions, pdata) {
  divs <- dividends_received(positions, pdata)
  h <- data.table::rbindlist(lapply(positions, function(p) {
    px <- pdata[id == paste0("px_", p$id), .(date, precio = value)][order(date)]
    if (!nrow(px)) { log_msg("WARN no prices for a tracked position"); return(NULL) }
    lots <- adjust_lots(p, pdata[id == paste0("spl_", p$id)])
    lots[, fx := fx_at(pdata, p$currency, date)]
    lots[, flow := shares * price * fx]               # + buy, - sale (EUR)
    x <- px[date >= min(lots$date)]
    x[, fx := fx_at(pdata, p$currency, date)]
    x[, unidad_eur := precio * fx]
    cum <- function(v, d) sapply(x$date, function(t) sum(v[d <= t]))
    x[, titulos := cum(lots$shares, lots$date)]
    x[, invertido_eur := cum(pmax(lots$flow, 0) + lots$fees * (lots$shares > 0), lots$date)]
    x[, ventas_eur := cum(pmax(-lots$flow, 0) - lots$fees * (lots$shares < 0), lots$date)]
    dv <- if (!is.null(divs) && nrow(divs)) divs[id == p$id] else NULL
    x[, dividendos_eur := if (is.null(dv) || !nrow(dv)) 0 else cum(dv$neto_eur, dv$fecha_ex)]
    x[, valor_eur := titulos * unidad_eur]
    x[, resultado_eur := valor_eur + ventas_eur + dividendos_eur - invertido_eur]
    x[, `:=`(id = p$id, nombre = p$name)]
    x[, .(date, id, nombre, titulos, precio, unidad_eur, valor_eur, invertido_eur,
          ventas_eur, dividendos_eur, resultado_eur)]
  }))
  if (!nrow(h)) return(h)
  # Common calendar: on a market holiday a position keeps its last value, so totals
  # across exchanges (Madrid, New York...) do not drop
  first <- h[, .(start = min(date)), by = id]
  grid <- first[, .(date = sort(unique(h$date))[sort(unique(h$date)) >= start]), by = id]
  h[grid, on = c("id", "date"), roll = TRUE]
}

# Average-cost book of a position: remaining cost and realised gain (EUR)
lot_book <- function(lots) {
  shares <- 0; cost <- 0; realised <- 0
  for (i in seq_len(nrow(lots))) {
    l <- lots[i]
    if (l$shares > 0) {
      shares <- shares + l$shares; cost <- cost + l$flow + l$fees
    } else {
      out <- cost * (-l$shares / shares)
      realised <- realised + (-l$flow - l$fees) - out
      shares <- shares + l$shares; cost <- cost - out
    }
  }
  list(shares = shares, cost = cost, realised = realised)
}

# Latest snapshot per position + total row
position_summary <- function(positions, pdata, hist) {
  last_on <- function(x, d) { y <- x[date <= d]; if (nrow(y)) data.table::last(y$unidad_eur) else NA_real_ }
  rows <- lapply(positions, function(p) {
    x <- hist[id == p$id][order(date)]
    if (!nrow(x)) return(NULL)
    lots <- adjust_lots(p, pdata[id == paste0("spl_", p$id)])
    lots[, fx := fx_at(pdata, p$currency, date)][, flow := shares * price * fx]
    b <- lot_book(lots)
    l <- data.table::last(x); d <- l$date
    ytd_ref <- last_on(x, as.Date(paste0(format(d, "%Y"), "-01-01")) - 1)
    data.table::data.table(
      id = p$id, nombre = p$name, fecha = d, divisa = quote_ccy(p$currency),
      titulos = l$titulos, precio = l$precio,
      valor_eur = l$valor_eur, coste_eur = b$cost,
      precio_medio_eur = if (b$shares > 0) b$cost / b$shares else NA_real_,
      caida_max_pct = 100 * (1 - l$unidad_eur / max(x$unidad_eur)),   # from high since purchase
      plusvalia_latente_eur = l$valor_eur - b$cost, plusvalia_realizada_eur = b$realised,
      dividendos_eur = l$dividendos_eur, resultado_eur = l$resultado_eur,
      rentab_total_pct = 100 * l$resultado_eur / l$invertido_eur,
      var_dia_pct = 100 * (l$unidad_eur / last_on(x, d - 1) - 1),
      var_1m_pct  = 100 * (l$unidad_eur / last_on(x, d - 30) - 1),
      var_anio_pct = 100 * (l$unidad_eur / ytd_ref - 1))
  })
  s <- data.table::rbindlist(rows)
  if (!nrow(s)) return(s)
  s[, peso_pct := 100 * valor_eur / sum(valor_eur)]
  # Total: changes of the open positions, weighted by value (price only, no dividends)
  wavg <- function(v, w) { ok <- !is.na(v) & w > 0; if (any(ok)) sum(v[ok] * w[ok]) / sum(w[ok]) else NA_real_ }
  tot <- s[, .(id = "total", nombre = "Total", fecha = max(fecha), divisa = NA_character_,
               titulos = NA_real_, precio = NA_real_, valor_eur = sum(valor_eur),
               coste_eur = sum(coste_eur), precio_medio_eur = NA_real_, caida_max_pct = NA_real_,
               plusvalia_latente_eur = sum(plusvalia_latente_eur),
               plusvalia_realizada_eur = sum(plusvalia_realizada_eur),
               dividendos_eur = sum(dividendos_eur), resultado_eur = sum(resultado_eur),
               rentab_total_pct = NA_real_,
               var_dia_pct = wavg(var_dia_pct, valor_eur), var_1m_pct = wavg(var_1m_pct, valor_eur),
               var_anio_pct = wavg(var_anio_pct, valor_eur), peso_pct = 100)]
  inv <- hist[, .SD[.N], by = id][, sum(invertido_eur)]
  tot[, rentab_total_pct := 100 * resultado_eur / inv]
  rbind(s, tot)
}
