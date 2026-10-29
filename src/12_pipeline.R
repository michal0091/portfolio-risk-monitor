# Whole pipeline as a function, shared by main.R (local, console summary) and report.R
# (Telegram, quiet). Writes every output to cfg$project$output and returns the results.

run_pipeline <- function(cfg, pf, pos, charts = TRUE) {
  out <- cfg$project$output
  synthetic <- isTRUE(cfg$project$use_synthetic)
  fw <- function(x, name) data.table::fwrite(x, file.path(out, name))

  # 1. Data
  daily <- if (synthetic) {
    log_msg("Synthetic mode: simulated data, NOT market data")
    synthetic_daily(cfg)
  } else {
    download_all(cfg)
  }
  data.table::fwrite(daily, file.path(cfg$project$data_processed, "daily.csv"))

  # 2. Regimes
  w <- label_regimes(build_monthly(daily), cfg)
  perf <- regime_performance(w, c("spx", "stoxx50", "sox", "kospi", "world", "europe", "em", "gold"))
  fw(w[, .(m_y = format(m_y), infl_yoy, credito = get(cfg$regimes$credit_series), regime)],
     "regimes_monthly.csv")
  fw(perf, "regime_performance.csv")
  fw(regime_transitions(w), "regime_transitions.csv")

  # 3. Stress
  sc <- scenario_table(cfg, pf)
  ph <- portfolio_history(daily, pf)
  fw(sc, "scenarios.csv")
  fw(equity_share_grid(cfg, pf), "equity_share_grid.csv")
  fw(historical_stress(daily, cfg, pf), "historical_stress.csv")
  fw(ph, "portfolio_history.csv")

  # 4. Signals
  sig  <- signal_table(stats::na.omit(daily), cfg)
  hint <- scenario_hint(sig)
  fw(sig, "signals.csv")
  fw(hint, "scenario_hint.csv")

  # 5. Rebalance
  reb <- rebalance_plan(pf)
  fw(reb, "rebalance_plan.csv")
  fw(glide_path(pf), "glide_path.csv")

  # 6. Tracked positions
  pdata <- if (synthetic) synthetic_positions(pos) else download_positions(pos, cfg)
  pos_hist <- position_history(pos, pdata)
  pos_sum  <- position_summary(pos, pdata, pos_hist)
  divs     <- dividends_received(pos, pdata)
  fw(pos_hist, "positions_history.csv")
  fw(pos_sum, "positions_summary.csv")
  fw(divs, "dividends.csv")

  # 7. Charts
  if (charts) {
    invisible(plot_signals(daily, cfg, file = file.path(out, "signals.png")))
    invisible(plot_regimes(w, file = file.path(out, "regimes.png")))
    invisible(plot_scenarios(sc, cfg, file = file.path(out, "scenarios.png")))
    invisible(plot_positions(pos_hist, file = file.path(out, "positions.png")))
    invisible(plot_portfolio(ph, cfg, pf$rebalance$frequency_months,
                             file = file.path(out, "portfolio.png")))
  }

  list(daily = daily, regimes = w, scenarios = sc, portfolio = ph, signals = sig,
       hint = hint, rebalance = reb, positions = pos_sum, positions_history = pos_hist,
       dividends = divs, stale = c(attr(daily, "stale"), attr(pdata, "stale")))
}
