# Telegram report - entry point
# Usage: Rscript report.R daily|weekly [--dry-run] [--synthetic] [--cron "<schedule>"]
# GitHub Actions passes the schedule that fired with --cron; the report only goes out when
# that cron matches the local Madrid time (two UTC crons cover summer and winter time).
# Prints nothing personal: public CI logs only show log_msg lines without figures.

suppressPackageStartupMessages({
  library(data.table)
})
for (f in sort(list.files("src", pattern = "\\.R$", full.names = TRUE))) source(f)

args <- commandArgs(trailingOnly = TRUE)
kind <- if (length(args) && args[1] %in% c("daily", "weekly")) args[1] else
  stop("usage: Rscript report.R daily|weekly [--dry-run] [--synthetic] [--cron \"<schedule>\"]")
dry_run <- "--dry-run" %in% args
cron <- if ("--cron" %in% args) args[match("--cron", args) + 1] else ""
if (is.na(cron)) cron <- ""

local_hour <- c(daily = 18, weekly = 10)[[kind]]
if (!scheduled_now(cron, local_hour)) {
  log_msg("Scheduled run skipped: this cron is not ", local_hour, ":xx in Europe/Madrid today")
  quit(save = "no", status = 0)
}

cfg <- read_cfg()
pf  <- read_portfolio()
pos <- read_positions()
if ("--synthetic" %in% args) cfg$project$use_synthetic <- TRUE
ensure_dirs(cfg)
state <- file.path(cfg$project$output, "state")
dir.create(state, showWarnings = FALSE)

r <- run_pipeline(cfg, pf, pos, charts = kind == "weekly",
                  forecast = if (kind == "weekly") "full" else "daily")

read_state <- function(name) {
  f <- file.path(state, name)
  if (file.exists(f)) fread(f, sep = ",", colClasses = "character") else NULL
}
prev_sig <- read_state("signals_prev.csv")
seen_div <- read_state("dividends_seen.csv")
new_divs <- new_dividends(r$dividends, seen_div)

text <- if (kind == "daily") build_daily_report(r, prev_sig, new_divs) else
  build_weekly_report(r, prev_sig, new_divs)
out <- cfg$project$output
photos <- if (kind == "weekly") c(
  "Semáforo" = file.path(out, "signals.png"),
  "Cartera de fondos" = file.path(out, "portfolio.png"),
  "Posiciones" = file.path(out, "positions.png"),
  "Escenarios" = file.path(out, "scenarios.png"),
  "Riesgo de caída (bootstrap)" = file.path(out, "bootstrap.png"),
  "Volatilidad (GARCH)" = file.path(out, "garch.png")) else character()
send_report(text, kind, cfg, photos, dry_run)

# State for the next run (written only after the report went out or was saved)
fwrite(r$signals[, .(id, estado)], file.path(state, "signals_prev.csv"))
if (!is.null(r$dividends) && nrow(r$dividends)) {
  fwrite(unique(rbind(seen_div, data.table(clave = dividend_key(r$dividends)))),
         file.path(state, "dividends_seen.csv"))
}
log_msg("Done (", kind, ")")
