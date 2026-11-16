# Macro regimes: inflation (baja/alta) x credit (relajado/tensionado) --------------
# Monthly. Output: regime per month + conditional performance of each asset.

build_monthly <- function(daily) {
  m <- to_monthly(daily)
  w <- data.table::dcast(m, m_y ~ id, value.var = "value")
  w[, m_y := zoo::as.yearmon(as.numeric(m_y))]  # dcast drops the yearmon class
  w[]
}

label_regimes <- function(wide, cfg) {
  rg <- cfg$regimes
  w <- data.table::copy(wide)[order(m_y)]
  # Fill isolated gaps inside the series (e.g. US CPI Oct 2025, not published during the
  # shutdown); leading/trailing NAs (months not yet released) stay NA
  infl_idx <- zoo::na.approx(w[[rg$inflation_series]], na.rm = FALSE)
  w[, infl_yoy := 100 * (infl_idx / data.table::shift(infl_idx, 12) - 1)]
  cr <- w[[rg$credit_series]]
  w[, credit_med := zoo::rollapplyr(cr, rg$credit_tense_window, median, na.rm = TRUE,
                                     fill = NA, partial = 24)]
  # Hysteresis: inflation turns "alta" above inflation_high and stays so until it falls
  # below inflation_exit, so the regime does not flip each time it crosses one level
  exit <- if (is.null(rg$inflation_exit)) rg$inflation_high else rg$inflation_exit
  high <- logical(nrow(w)); state <- FALSE
  for (i in seq_len(nrow(w))) {
    v <- w$infl_yoy[i]
    if (!is.na(v)) state <- if (state) v > exit else v > rg$inflation_high
    high[i] <- state
  }
  w[, inflation := data.table::fifelse(high, "Inflación alta", "Inflación baja")]
  # Second inflation series, shown in the report only (e.g. euro area HICP)
  if (!is.null(rg$inflation_info) && rg$inflation_info %in% names(w)) {
    idx2 <- zoo::na.approx(w[[rg$inflation_info]], na.rm = FALSE)
    w[, infl_yoy_info := 100 * (idx2 / data.table::shift(idx2, 12) - 1)]
  }
  w[, credit := data.table::fifelse(cr > rg$credit_tense_abs | cr > credit_med + rg$credit_tense_margin,
                                    "Crédito tensionado", "Crédito relajado")]
  w[, regime := paste(inflation, credit, sep = " · ")]
  w[is.na(infl_yoy) | is.na(cr) | is.na(credit) | is.na(inflation), regime := NA_character_]
  w
}

# Mean monthly return, annualised vol, hit rate and worst month by regime
regime_performance <- function(w, assets) {
  res <- lapply(assets, function(a) {
    if (!a %in% names(w)) return(NULL)
    x <- w[, .(m_y, regime, p = get(a))]
    x[, ret := 100 * (p / data.table::shift(p) - 1)]
    x[!is.na(ret) & !is.na(regime),
      .(asset = a, months = .N, mean_ret_m = mean(ret),
        vol_ann = sd(ret) * sqrt(12), pct_positive = 100 * mean(ret > 0),
        worst_month = min(ret)), by = regime]
  })
  data.table::rbindlist(res)[order(asset, regime)]
}

# Transition matrix between regimes (row = from, col = to), monthly probabilities
regime_transitions <- function(w) {
  x <- w[!is.na(regime), .(from = regime, to = data.table::shift(regime, -1))][!is.na(to)]
  tab <- x[, .N, by = .(from, to)]
  tab[, prob := N / sum(N), by = from]
  data.table::dcast(tab, from ~ to, value.var = "prob", fill = 0)
}
