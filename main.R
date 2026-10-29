# Portfolio risk monitor - entry point
# Usage: Rscript main.R            (real data from Yahoo + FRED)
#        Rscript main.R synthetic  (offline demo with simulated data)

suppressPackageStartupMessages({
  library(data.table)
})
for (f in sort(list.files("src", pattern = "\\.R$", full.names = TRUE))) source(f)

cfg <- read_cfg()
pf  <- read_portfolio()
pos <- read_positions()
args <- commandArgs(trailingOnly = TRUE)
if (length(args) && args[1] == "synthetic") cfg$project$use_synthetic <- TRUE
ensure_dirs(cfg)

r <- run_pipeline(cfg, pf, pos)

# Console summary -------------------------------------------------------------------
cat("\n== Escenario sugerido por las señales ==\n"); print(r$hint)
cat("\n== Semáforo ==\n"); print(r$signals[, .(senal, valor = round(valor, 2), estado)])
cat("\n== Escenarios (% , cartera sin emergencia) ==\n")
print(r$scenarios[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
cat("\n== Traspasos al objetivo ==\n"); print(r$rebalance)
cat("\n== Posiciones ==\n")
print(r$positions[, .(nombre, valor_eur = round(valor_eur), resultado_eur = round(resultado_eur),
                      rentab_total_pct = round(rentab_total_pct, 1),
                      var_dia_pct = round(var_dia_pct, 2))])
if (!is.null(r$forecast$bootstrap)) {
  cat("\n== Riesgo de caída (bootstrap, ", r$forecast$bootstrap$regime, ") ==\n", sep = "")
  print(r$forecast$bootstrap$summary[, .(cartera, horizonte_meses, prob_caida = round(prob_caida, 1),
                                         rentab_p05 = round(rentab_p05, 1))])
  cat("\n== Alerta temprana (logit) ==\n")
  print(r$forecast$logit$metrics[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
}
if (length(r$stale)) log_msg("WARN no fresh data for: ", paste(r$stale, collapse = ", "))
log_msg("Done. Outputs in ", cfg$project$output, "/")
