# Forecasting: regime block bootstrap, early-warning logit, CAPM and GARCH ----------
# Settings in cfg$forecast. Probabilities are model outputs on limited history (few
# crisis episodes), not forecasts to rely on alone.

# --- Monthly returns of the funds with long-history fallbacks ----------------------
# Fund proxies start in 2009-2014; before that the long index stands in (as in
# historical_stress). Cash: €STR, or cfg$forecast$cash_fallback_pct before 2019.
fund_monthly_returns <- function(w, cfg) {
  w <- w[order(m_y)]
  ret <- function(x) x / data.table::shift(x) - 1
  pick <- function(a, b) { r <- ret(w[[a]]); rb <- ret(w[[b]]); data.table::fifelse(is.na(r), rb, r) }
  cash <- data.table::shift(w$estr) / 100 / 12
  cash[is.na(cash)] <- cfg$forecast$cash_fallback_pct / 100 / 12
  x <- data.table::data.table(m_y = w$m_y, regime = w$regime, groupama = cash,
                              world = pick("world", "spx"), europe = pick("europe", "stoxx50"),
                              em = pick("em", "kospi"))
  x[stats::complete.cases(x[, .(world, europe, em)])]
}

# Monthly return series of the portfolios and Groupama buckets to simulate
portfolio_monthly_returns <- function(fr, pf) {
  fund_ids <- c("groupama", "world", "europe", "em")
  mix <- function(wt) { wt <- unlist(wt)[fund_ids]; as.matrix(fr[, ..fund_ids]) %*% (wt / sum(wt)) }
  eq_w <- unlist(pf$weights_target)[c("world", "europe", "em")]
  eq <- as.matrix(fr[, .(world, europe, em)]) %*% (eq_w / sum(eq_w))
  bucket <- function(share) share / 100 * eq + (1 - share / 100) * fr$groupama
  out <- data.table::data.table(m_y = fr$m_y, regime = fr$regime,
                                `Cartera actual` = mix(pf$weights_current)[, 1],
                                `Cartera objetivo` = mix(pf$weights_target)[, 1])
  b <- pf$buckets
  if (!is.null(b$house_deposit$equity_share)) out[, `Bolsillo entrada` := bucket(b$house_deposit$equity_share)[, 1]]
  if (!is.null(b$general$equity_share)) out[, `Ahorro general` := bucket(b$general$equity_share)[, 1]]
  out
}

# --- 1. Block bootstrap within the current regime ----------------------------------
# Blocks of `block` consecutive months start in months of the current regime; all
# portfolios share the same draws so they are comparable.
regime_bootstrap <- function(pr, cfg) {
  bc <- cfg$forecast$bootstrap
  set.seed(bc$seed)
  cur <- data.table::last(pr[!is.na(regime), regime])
  n <- nrow(pr); H <- bc$horizon; B <- bc$block
  starts <- which(pr$regime == cur & seq_len(n) <= n - B + 1)
  used <- cur
  if (length(starts) < bc$min_regime_months) { starts <- seq_len(n - B + 1); used <- "todos los regímenes" }
  nb <- ceiling(H / B)
  idx <- t(vapply(seq_len(bc$n_paths), function(i) {
    s <- sample(starts, nb, replace = TRUE)
    as.vector(outer(0:(B - 1), s, `+`))[seq_len(H)]
  }, integer(H)))
  series <- setdiff(names(pr), c("m_y", "regime"))
  paths <- lapply(stats::setNames(series, series), function(s) {
    r <- matrix(pr[[s]][idx], nrow = bc$n_paths)
    t(apply(1 + r, 1, cumprod))                     # paths x months, start value 1
  })
  maxdd <- function(v, h) {
    v <- cbind(1, v[, seq_len(h), drop = FALSE])
    apply(v, 1, function(p) max(1 - p / cummax(p)))
  }
  summary <- data.table::rbindlist(lapply(series, function(s) {
    v <- paths[[s]]
    data.table::rbindlist(lapply(c(6, 12, 24)[c(6, 12, 24) <= H], function(h) data.table::data.table(
      cartera = s, horizonte_meses = h,
      prob_caida = 100 * mean(maxdd(v, h) > bc$loss / 100),
      rentab_p05 = 100 * (stats::quantile(v[, h], .05) - 1),
      rentab_p50 = 100 * (stats::quantile(v[, h], .50) - 1),
      rentab_p95 = 100 * (stats::quantile(v[, h], .95) - 1))))
  }))
  summary[, `:=`(regimen = used, meses_muestra = length(starts))]
  fan <- data.table::rbindlist(lapply(series, function(s) {
    q <- apply(paths[[s]], 2, stats::quantile, c(.05, .25, .5, .75, .95))
    data.table::data.table(cartera = s, mes = c(0, seq_len(H)),
                           p05 = c(1, q[1, ]), p25 = c(1, q[2, ]), p50 = c(1, q[3, ]),
                           p75 = c(1, q[4, ]), p95 = c(1, q[5, ]))
  }))
  list(summary = summary, fan = fan, regime = used)
}

# --- 2. Early-warning logit ---------------------------------------------------------
auc <- function(p, y) {   # Mann-Whitney
  ok <- !is.na(p) & !is.na(y); p <- p[ok]; y <- y[ok]
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (!n1 || !n0) return(NA_real_)
  (sum(rank(p)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
brier <- function(p, y) { ok <- !is.na(p) & !is.na(y); mean((p[ok] - y[ok])^2) }

# Month-end features and target: drawdown of the index beyond `loss` within `horizon` months
logit_data <- function(daily, w, cfg) {
  lc <- cfg$forecast$logit
  px <- daily[id == lc$index & !is.na(value)][order(date)]
  me <- px[, .(date = data.table::last(date), p = data.table::last(value)), by = .(m_y = zoo::as.yearmon(date))]
  me[, end := zoo::as.Date(m_y + lc$horizon / 12, frac = 1)]
  me[, fwd_min := sapply(seq_len(.N), function(i) {
    f <- px[date > me$date[i] & date <= me$end[i], value]
    if (length(f)) min(f) else NA_real_
  })]
  me[end > max(px$date), fwd_min := NA_real_]          # future not observed yet
  me[, y := as.integer(fwd_min / p - 1 < -lc$loss / 100)]
  x <- w[, .(m_y, baa10y, curva = ust10 - ust2, tips10, vix,
             sox_3m = 100 * (sox / data.table::shift(sox, 3) - 1))]
  x[me[, .(m_y, y)], on = "m_y"][order(m_y)]
}

early_warning <- function(daily, w, cfg) {
  lc <- cfg$forecast$logit
  d <- logit_data(daily, w, cfg)
  vars <- c("baa10y", "curva", "tips10", "vix", "sox_3m")
  d <- d[stats::complete.cases(d[, ..vars])]
  fml <- stats::reformulate(vars, "y")
  fit_glm <- function(tr) suppressWarnings(stats::glm(fml, family = stats::binomial(), data = tr))
  # Expanding window: at month t only targets fully observed by t are used (t - horizon)
  d[, p_oos := NA_real_]
  for (i in which(zoo::as.Date(d$m_y) >= as.Date(lc$start_oos))) {
    tr <- d[seq_len(max(0, i - lc$horizon))][!is.na(y)]
    if (sum(tr$y) < lc$min_events || sum(1 - tr$y) < lc$min_events) next
    d$p_oos[i] <- stats::predict(fit_glm(tr), d[i], type = "response")
  }
  full <- fit_glm(d[!is.na(y)])
  ev <- d[!is.na(p_oos) & !is.na(y)]
  base <- d[!is.na(y), cumsum(y) / seq_len(.N)]          # climatology for the Brier skill
  metrics <- data.table::data.table(
    meses_oos = nrow(ev), eventos_oos = sum(ev$y),
    auc_oos = auc(ev$p_oos, ev$y), brier_oos = brier(ev$p_oos, ev$y),
    brier_clima = brier(data.table::shift(base)[match(ev$m_y, d[!is.na(y), m_y])], ev$y),
    prob_actual = 100 * stats::predict(full, d[.N], type = "response"),
    prob_actual_oos = 100 * data.table::last(d$p_oos), fecha = format(d[.N, m_y]))
  coefs <- data.table::as.data.table(summary(full)$coefficients, keep.rownames = "variable")
  list(metrics = metrics, coefs = coefs, series = d[, .(m_y = format(m_y), y, p_oos)])
}

# --- 3. CAPM ------------------------------------------------------------------------
# Monthly excess returns over €STR versus the market proxy, last `window` months
capm_table <- function(daily, prices, cfg) {
  cc <- cfg$forecast$capm
  me <- function(dt) dt[order(date), .(v = data.table::last(v)), by = .(m_y = zoo::as.yearmon(date))]
  series <- list()
  for (f in c("world", "europe", "em")) series[[f]] <- me(daily[id == f, .(date, v = value)])
  if (!is.null(prices) && nrow(prices)) {
    for (p in unique(prices$id)) series[[p]] <- me(prices[id == p, .(date, v = unidad_eur)])
  }
  rf <- me(daily[id == "estr", .(date, v = value)])[, .(m_y, rf = data.table::shift(v) / 1200)]
  mk <- series[[cc$market]][, .(m_y, rm = v / data.table::shift(v) - 1)]
  names_map <- c(world = "MSCI World (proxy)", europe = "Europa (proxy)", em = "Emergentes (proxy)")
  if (!is.null(prices)) names_map <- c(names_map, unique(prices[, .(id, nombre)])[, stats::setNames(nombre, id)])
  data.table::rbindlist(lapply(setdiff(names(series), cc$market), function(s) {
    x <- series[[s]][, .(m_y, ri = v / data.table::shift(v) - 1)][mk, on = "m_y"][rf, on = "m_y"]
    x <- utils::tail(x[stats::complete.cases(x)], cc$window)
    if (nrow(x) < 12) return(NULL)
    m <- stats::lm(I(ri - rf) ~ I(rm - rf), data = x)
    data.table::data.table(id = s, nombre = names_map[[s]], meses = nrow(x),
                           alfa_anual_pct = 1200 * stats::coef(m)[[1]], beta = stats::coef(m)[[2]],
                           r2 = suppressWarnings(summary(m)$r.squared),   # market itself: perfect fit
                           vol_anual_pct = 100 * stats::sd(x$ri) * sqrt(12))
  }))
}

# --- 4. GARCH -----------------------------------------------------------------------
# GJR-GARCH(1,1) with Student-t errors on daily log returns (%). VaR/ES as positive losses.
garch_one <- function(price, label, cfg) {
  r <- 100 * diff(log(price[!is.na(price)]))
  r <- r[is.finite(r)]
  if (length(r) < cfg$forecast$garch$min_obs) return(NULL)
  spec <- rugarch::ugarchspec(
    variance.model = list(model = cfg$forecast$garch$model, garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = "std")
  fit <- tryCatch(rugarch::ugarchfit(spec, r, solver = "hybrid"), error = function(e) NULL)
  if (is.null(fit) || fit@fit$convergence != 0) return(NULL)
  fc <- rugarch::ugarchforecast(fit, n.ahead = 10)
  sig <- as.numeric(rugarch::sigma(fc)); mu <- as.numeric(rugarch::fitted(fc))
  nu <- rugarch::coef(fit)[["shape"]]
  hist_sig <- as.numeric(rugarch::sigma(fit))
  tail_loss <- function(p, s, m) {
    q <- rugarch::qdist("std", p, mu = 0, sigma = 1, shape = nu)
    es <- stats::integrate(function(z) z * rugarch::ddist("std", z, mu = 0, sigma = 1, shape = nu),
                           -Inf, q)$value / p
    c(var = -(m + s * q), es = -(m + s * es))
  }
  pers <- sum(rugarch::coef(fit)[c("alpha1", "beta1")]) + rugarch::coef(fit)[["gamma1"]] / 2
  s10 <- sqrt(sum(sig^2)); m10 <- sum(mu)   # 10-day: sum of variances, t quantile (approx.)
  v95 <- tail_loss(.05, sig[1], mu[1]); v99 <- tail_loss(.01, sig[1], mu[1])
  v95_10 <- tail_loss(.05, s10, m10)
  list(table = data.table::data.table(
    serie = label, obs = length(r), vol_anual_pct = sig[1] * sqrt(252),
    percentil_vol = 100 * mean(hist_sig <= sig[1]),
    var95_1d_pct = v95[["var"]], es95_1d_pct = v95[["es"]],
    var99_1d_pct = v99[["var"]], es99_1d_pct = v99[["es"]], var95_10d_pct = v95_10[["var"]],
    persistencia = pers,
    media_vida_dias = if (pers < 1) log(.5) / log(pers) else Inf,   # days for a shock to halve
    vol_hace_1s_pct = hist_sig[max(1, length(hist_sig) - 4)] * sqrt(252),   # 5 sessions earlier
    # last daily move in conditional standard deviations (sigma known before that day)
    ret_ultimo_pct = r[length(r)],
    z_ultimo = (r[length(r)] - rugarch::coef(fit)[["mu"]]) / hist_sig[length(hist_sig)],
    grados_libertad = nu),
    vol = data.table::data.table(serie = label, n = seq_along(hist_sig),
                                 vol_anual_pct = hist_sig * sqrt(252)))
}

# Fund portfolios, today's positions as a whole and each position (full price history)
garch_table <- function(ph, prices, pos_sum, cfg) {
  jobs <- list()
  add <- function(label, id, date, price) jobs[[length(jobs) + 1]] <<- list(label = label, id = id,
                                                                           date = date, price = price)
  for (cart in unique(ph$cartera)) { x <- ph[cartera == cart][order(date)]; add(cart, cart, x$date, x$valor) }
  if (!is.null(pos_sum) && nrow(pos_sum)) {
    idx <- positions_index(prices, pos_sum)
    if (!is.null(idx)) add("Tus posiciones", "posiciones", idx$date, idx$valor)
    for (p in pos_sum[id != "total" & titulos > 0, id]) {
      x <- prices[id == p][order(date)]
      if (nrow(x)) add(x$nombre[1], p, x$date, x$unidad_eur)
    }
  }
  res <- lapply(jobs, function(j) {
    g <- garch_one(j$price, j$label, cfg)
    if (is.null(g)) return(NULL)
    g$vol[, date := utils::tail(j$date, nrow(g$vol))]
    g$table[, id := j$id]
    g
  })
  res <- res[!sapply(res, is.null)]
  list(table = data.table::rbindlist(lapply(res, `[[`, "table")),
       vol = data.table::rbindlist(lapply(res, `[[`, "vol"))[, n := NULL])
}

# Share of the positions' variance due to each position (Euler: w_i (S w)_i / w' S w),
# with the covariance of the last `risk_window` daily returns
risk_contribution <- function(prices, pos_sum, cfg) {
  h <- pos_sum[id != "total" & titulos > 0, .(id, nombre, valor_eur)]
  if (nrow(h) < 2) return(NULL)
  w <- data.table::dcast(prices[id %in% h$id], date ~ id, value.var = "unidad_eur")[order(date)]
  for (p in h$id) w[, (p) := zoo::na.locf(get(p), na.rm = FALSE)]
  w <- utils::tail(w[stats::complete.cases(w)], cfg$forecast$garch$risk_window + 1)
  if (nrow(w) < 60) return(NULL)
  m <- as.matrix(w[, h$id, with = FALSE])
  r <- diff(log(m))
  S <- stats::cov(r)
  wt <- h$valor_eur / sum(h$valor_eur)
  contrib <- wt * as.numeric(S %*% wt) / as.numeric(t(wt) %*% S %*% wt)
  data.table::data.table(id = h$id, nombre = h$nombre, peso_pct = 100 * wt,
                         aporte_riesgo_pct = 100 * contrib)[order(-aporte_riesgo_pct)]
}

# --- All forecasts ------------------------------------------------------------------
# level "daily": GARCH only (portfolios and positions, a few seconds); "full": everything.
# prices: full price history of the positions (position_prices); pos_sum: position_summary
run_forecasts <- function(daily, w, ph, prices, pos_sum, cfg, pf, level = "full") {
  out <- list(garch = garch_table(ph, prices, pos_sum, cfg))   # positions too: daily move alerts
  if (level == "full") {
    pr <- portfolio_monthly_returns(fund_monthly_returns(w, cfg), pf)
    out$bootstrap <- regime_bootstrap(pr, cfg)
    out$logit <- early_warning(daily, w, cfg)
    out$capm <- capm_table(daily, prices, cfg)
    out$risk <- risk_contribution(prices, pos_sum, cfg)
  }
  out
}
