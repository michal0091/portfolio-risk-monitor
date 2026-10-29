# Scenario and historical stress tests ---------------------------------------------

scenario_table <- function(cfg, pf) {
  ret <- cfg$scenarios$returns
  sc  <- names(cfg$scenarios$probabilities)
  fund_ids <- c("groupama", "world", "europe", "em")
  emergency <- pf$buckets$emergency$groupama
  pp <- pf$pension_plan
  share_pp <- pp$share_of_total / 100

  port_ret <- function(w, s, excl_emergency = TRUE) {
    w <- unlist(w)[fund_ids]
    if (excl_emergency) w["groupama"] <- w["groupama"] - emergency
    sum(w * sapply(fund_ids, function(f) ret[[f]][[s]])) / sum(w)
  }
  pp_ret <- function(s) {
    sl <- unlist(pp$sleeves)
    sum(sl * sapply(names(sl), function(k) ret[[k]][[s]])) / sum(sl)
  }
  rows <- lapply(sc, function(s) {
    cur <- port_ret(pf$weights_current, s); tgt <- port_ret(pf$weights_target, s)
    ppr <- pp_ret(s)
    # combined investable wealth = fund portfolio ex-emergency + pension plan
    inv_f <- (1 - share_pp) * (100 - emergency) / 100
    comb  <- (inv_f * tgt + share_pp * ppr) / (inv_f + share_pp)
    data.table::data.table(
      scenario = s, prob = cfg$scenarios$probabilities[[s]],
      fondos_actual = cur, fondos_objetivo = tgt, plan_pensiones = ppr,
      conjunto_objetivo = comb)
  })
  dt <- data.table::rbindlist(rows)
  ev <- dt[, lapply(.SD, function(x) sum(x * prob)), .SDcols = -c("scenario", "prob")]
  rbind(dt, cbind(data.table::data.table(scenario = "Esperado", prob = 1), ev))
}

# Loss of the target portfolio by equity share (for the -15 % tolerance check)
equity_share_grid <- function(cfg, pf, shares = seq(0.30, 0.90, by = 0.05)) {
  ret <- cfg$scenarios$returns; w <- unlist(pf$weights_target)[c("world", "europe", "em")]
  sc <- names(cfg$scenarios$probabilities)
  data.table::rbindlist(lapply(shares, function(e) {
    eq <- sapply(sc, function(s) sum(w * sapply(names(w), function(f) ret[[f]][[s]])) / sum(w))
    cash <- sapply(sc, function(s) ret$groupama[[s]])
    data.table::data.table(equity_share = 100 * e, scenario = sc, loss = e * eq + (1 - e) * cash)
  }))
}

# Peak-to-trough of each proxy inside historical windows; portfolio uses target weights
historical_stress <- function(daily, cfg, pf) {
  ids <- c(world = "world", europe = "europe", em = "em", spx = "spx",
           stoxx50 = "stoxx50", sox = "sox", kospi = "kospi", gold = "gold")
  res <- data.table::rbindlist(lapply(cfg$stress_windows, function(win) {
    d <- daily[id %in% ids & date >= as.Date(win$start) & date <= as.Date(win$end)][order(date)]
    if (!nrow(d)) return(NULL)
    d[, .(window = win$name, start_to_end = 100 * (data.table::last(value) / value[1] - 1),
          max_drawdown = -max(drawdown_pct(value), na.rm = TRUE)), by = id]
  }))
  # Portfolio estimate: fund proxy if it existed, otherwise long-history index
  fallback <- c(world = "spx", europe = "stoxx50", em = "kospi")
  w <- unlist(pf$weights_target)
  inv <- w - c(groupama = pf$buckets$emergency$groupama, world = 0, europe = 0, em = 0)
  port <- res[, {
    r <- sapply(names(fallback), function(f) {
      v <- start_to_end[id == f]
      if (!length(v)) v <- start_to_end[id == fallback[[f]]]
      if (!length(v)) NA_real_ else v
    })
    .(id = "cartera_objetivo_sin_emergencia",
      start_to_end = sum(inv[names(r)] * r, na.rm = TRUE) / sum(inv),
      max_drawdown = NA_real_)
  }, by = window]
  rbind(res, port)
}
