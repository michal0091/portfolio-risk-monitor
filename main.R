# Portfolio risk monitor - entry point
# Usage: Rscript main.R            (real data from Yahoo + FRED)
#        Rscript main.R synthetic  (offline demo with simulated data)

suppressPackageStartupMessages({
  library(data.table)
})
for (f in sort(list.files("src", pattern = "\\.R$", full.names = TRUE))) source(f)

cfg <- read_cfg()
pf  <- read_portfolio()
args <- commandArgs(trailingOnly = TRUE)
if (length(args) && args[1] == "synthetic") cfg$project$use_synthetic <- TRUE
ensure_dirs(cfg)
out <- cfg$project$output

# 1. Data ---------------------------------------------------------------------------
daily <- if (isTRUE(cfg$project$use_synthetic)) {
  log_msg("Synthetic mode: simulated data, NOT market data")
  synthetic_daily(cfg)
} else {
  download_all(cfg)
}
fwrite(daily, file.path(cfg$project$data_processed, "daily.csv"))

# 2. Regimes ------------------------------------------------------------------------
wide <- build_monthly(daily)
w    <- label_regimes(wide, cfg)
perf <- regime_performance(w, c("spx", "stoxx50", "sox", "kospi", "world", "europe", "em", "gold"))
trans <- regime_transitions(w)
fwrite(w[, .(m_y = format(m_y), infl_yoy, credito = get(cfg$regimes$credit_series), regime)],
       file.path(out, "regimes_monthly.csv"))
fwrite(perf, file.path(out, "regime_performance.csv"))
fwrite(trans, file.path(out, "regime_transitions.csv"))

# 3. Stress -------------------------------------------------------------------------
sc   <- scenario_table(cfg, pf)
grid <- equity_share_grid(cfg, pf)
hist <- historical_stress(daily, cfg, pf)
fwrite(sc, file.path(out, "scenarios.csv"))
fwrite(grid, file.path(out, "equity_share_grid.csv"))
fwrite(hist, file.path(out, "historical_stress.csv"))

# 4. Signals ------------------------------------------------------------------------
sig  <- signal_table(na.omit(daily), cfg)
hint <- scenario_hint(sig)
fwrite(sig, file.path(out, "signals.csv"))
fwrite(hint, file.path(out, "scenario_hint.csv"))

# 5. Rebalance ----------------------------------------------------------------------
reb <- rebalance_plan(pf)
fwrite(reb, file.path(out, "rebalance_plan.csv"))
fwrite(glide_path(pf), file.path(out, "glide_path.csv"))

# 6. Charts -------------------------------------------------------------------------
invisible(plot_signals(daily, cfg, file = file.path(out, "signals.png")))
invisible(plot_regimes(w, file = file.path(out, "regimes.png")))
invisible(plot_scenarios(sc, cfg, file = file.path(out, "scenarios.png")))

# 7. Console summary ----------------------------------------------------------------
cat("\n== Escenario sugerido por las señales ==\n"); print(hint)
cat("\n== Semáforo ==\n"); print(sig[, .(senal, valor = round(valor, 2), estado)])
cat("\n== Escenarios (% , cartera sin emergencia) ==\n")
print(sc[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
cat("\n== Traspasos al objetivo ==\n"); print(reb)
log_msg("Done. Outputs in ", out, "/")
