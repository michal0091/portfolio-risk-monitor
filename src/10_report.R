# Telegram reports (HTML parse mode) ---------------------------------------------------
# Daily: traffic light + changes, fund portfolio, positions, new dividends.
# Weekly: daily content plus regime, scenarios, rebalance, full positions and charts.

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

fmt_num <- function(x, digits = 0) {
  ifelse(is.na(x), "-", formatC(x, format = "f", digits = digits, big.mark = ".",
                                decimal.mark = ","))
}
fmt_signed <- function(x, digits = 1, suffix = " %") {
  ifelse(is.na(x), "-", paste0(ifelse(x > 0, "+", ""), fmt_num(x, digits), suffix))
}
fmt_eur <- function(x, signed = FALSE) {
  if (signed) fmt_signed(x, 0, " €") else paste0(fmt_num(x, 0), " €")
}

status_icon <- c("Verde" = "\U0001F7E2", "Ámbar" = "\U0001F7E1", "Rojo" = "\U0001F534")

# Change of each signal versus the previous run (NULL if there is no previous state)
signal_changes <- function(sig, prev) {
  if (is.null(prev) || !nrow(prev)) return(sig[0, .(senal, antes = estado, ahora = estado)])
  x <- prev[, .(id, antes = estado)][sig[, .(id, senal, ahora = estado)], on = "id"]
  x[!is.na(antes) & antes != ahora, .(senal, antes, ahora)]
}

report_header <- function(title, r) {
  d <- max(r$signals$fecha)
  paste0("<b>", html_escape(title), "</b> · ", format(Sys.Date(), "%d/%m/%Y"),
         "\n<i>Datos de mercado hasta el ", format(d, "%d/%m/%Y"), "</i>")
}

section_signals <- function(r, prev, full = FALSE) {
  sig <- r$signals
  n <- table(factor(sig$estado, levels = names(status_icon)))
  lines <- c(paste0("<b>Semáforo</b>  ", paste(status_icon[names(n)], n, collapse = "  ")),
             paste0("Escenario más coherente: ", html_escape(r$hint$escenario_probable)))
  show <- if (full) sig else sig[estado != "Verde"]
  show <- show[order(-match(estado, names(status_icon)))]
  if (nrow(show)) lines <- c(lines, paste0(status_icon[show$estado], " ", html_escape(show$senal),
                                           ": ", fmt_num(show$valor, 2)))
  ch <- signal_changes(sig, prev)
  if (nrow(ch)) lines <- c(lines, "<b>Cambios desde el último informe</b>",
                           paste0(html_escape(ch$senal), ": ", ch$antes, " → ", ch$ahora))
  paste(lines, collapse = "\n")
}

section_funds <- function(r) {
  ph <- r$portfolio[cartera == "Cartera actual"][order(date)]
  if (nrow(ph) < 2) return(NULL)
  l <- ph[.N]
  ref <- function(d) { y <- ph[date <= d]; if (nrow(y)) data.table::last(y$valor) else NA_real_ }
  ytd <- ref(as.Date(paste0(format(l$date, "%Y"), "-01-01")) - 1)
  paste0("<b>Cartera de fondos</b> (simulada con proxies)\n",
         "Día ", fmt_signed(100 * (l$valor / ph[.N - 1, valor] - 1), 2),
         " · Año ", fmt_signed(100 * (l$valor / ytd - 1)),
         " · Caída desde máximo ", fmt_signed(l$caida_desde_max))
}

section_positions <- function(r, full = FALSE) {
  s <- r$positions
  if (is.null(s) || !nrow(s)) return(NULL)
  tot <- s[id == "total"]; p <- s[id != "total"]
  lines <- c("<b>Posiciones</b>",
             paste0("Valor ", fmt_eur(tot$valor_eur), " · Día ", fmt_signed(tot$var_dia_pct, 2),
                    " (", fmt_eur(tot$valor_eur * tot$var_dia_pct / (100 + tot$var_dia_pct), TRUE), ")"),
             paste0("Resultado total ", fmt_eur(tot$resultado_eur, TRUE), " (",
                    fmt_signed(tot$rentab_total_pct), ")"))
  p <- p[titulos > 0]
  if (!full) p <- p[order(-abs(var_dia_pct))][seq_len(min(3, .N))]
  if (nrow(p)) {
    lines <- c(lines, if (full) "" else "Mayores movimientos del día:")
    lines <- c(lines, if (full) paste0(
      "• ", html_escape(p$nombre), ": ", fmt_eur(p$valor_eur), " · día ", fmt_signed(p$var_dia_pct, 2),
      " · mes ", fmt_signed(p$var_1m_pct), " · año ", fmt_signed(p$var_anio_pct),
      " · total ", fmt_signed(p$rentab_total_pct), " (div. ", fmt_eur(p$dividendos_eur), ")")
    else paste0("• ", html_escape(p$nombre), " ", fmt_signed(p$var_dia_pct, 2)))
  }
  paste(lines, collapse = "\n")
}

# Dividends not reported before (keys stored in the state folder). Without state (first
# run) only the last `days` days count as new, not the whole history.
dividend_key <- function(divs) paste(divs$id, format(divs$fecha_ex), sep = "|")

new_dividends <- function(divs, seen, days = 7, today = Sys.Date()) {
  if (is.null(divs) || !nrow(divs)) return(divs)
  divs <- data.table::copy(divs)[, clave := dividend_key(divs)]
  divs <- if (!is.null(seen) && nrow(seen)) divs[!clave %in% seen$clave] else divs[fecha_ex >= today - days]
  divs[order(fecha_ex)]
}

section_dividends <- function(nd) {
  if (is.null(nd) || !nrow(nd)) return(NULL)
  paste(c("<b>Dividendos nuevos</b>",
          paste0("• ", html_escape(nd$nombre), " (ex ", format(nd$fecha_ex, "%d/%m/%Y"), "): ",
                 fmt_num(nd$neto_eur, 2), " € netos")), collapse = "\n")
}

section_stale <- function(r) {
  if (!length(r$stale)) return(NULL)
  paste0("⚠️ Sin datos nuevos (se usa la caché): ", html_escape(paste(r$stale, collapse = ", ")))
}

section_regime <- function(r) {
  w <- r$regimes[!is.na(regime)][order(m_y)]
  l <- data.table::last(w)
  paste0("<b>Régimen macro</b> (", format(l$m_y, "%m/%Y"), "): ", html_escape(l$regime),
         "\nInflación EE. UU. ", fmt_signed(l$infl_yoy), " interanual")
}

section_scenarios <- function(r) {
  sc <- r$scenarios
  paste(c("<b>Escenarios</b> (fondos objetivo · conjunto con plan)",
          paste0(sc$scenario, ": ", fmt_signed(sc$fondos_objetivo), " · ",
                 fmt_signed(sc$conjunto_objetivo))), collapse = "\n")
}

section_rebalance <- function(r) {
  reb <- r$rebalance[fuera_de_banda == TRUE]
  if (!nrow(reb)) return("<b>Rebalanceo</b>: todos los fondos dentro de banda")
  paste(c("<b>Rebalanceo</b> (fondos fuera de banda)",
          paste0("• ", html_escape(reb$nombre), ": ", fmt_signed(reb$desvio_pp, 1, " pp"),
                 " → traspasar ", fmt_signed(reb$traspaso_pp, 1, " pp"))), collapse = "\n")
}

build_daily_report <- function(r, prev_signals = NULL, new_divs = NULL) {
  parts <- c(report_header("Monitor de riesgo · resumen diario", r),
             section_stale(r), section_signals(r, prev_signals), section_funds(r),
             section_positions(r), section_dividends(new_divs), r$extra_daily)
  paste(parts[!is.na(parts) & nzchar(parts)], collapse = "\n\n")
}

build_weekly_report <- function(r, prev_signals = NULL, new_divs = NULL) {
  parts <- c(report_header("Monitor de riesgo · informe semanal", r),
             section_stale(r), section_regime(r), section_signals(r, prev_signals, full = TRUE),
             section_funds(r), section_scenarios(r), section_rebalance(r),
             section_positions(r, full = TRUE), section_dividends(new_divs), r$extra_weekly)
  paste(parts[!is.na(parts) & nzchar(parts)], collapse = "\n\n")
}

# Should a scheduled run send now? `cron` is the GitHub schedule that fired ("30 16 * * 1-5");
# empty = manual or local run, always send. The cron's UTC hour must equal the wanted
# Madrid hour minus today's UTC offset (+1 in winter, +2 in summer).
scheduled_now <- function(cron, local_hour, now = Sys.time()) {
  if (!nzchar(cron)) return(TRUE)
  utc_hour <- as.integer(strsplit(trimws(cron), "\\s+")[[1]][2])
  offset <- as.POSIXlt(now, tz = "Europe/Madrid")$gmtoff / 3600
  utc_hour == local_hour - offset
}

# Split a message into chunks under Telegram's 4096-character limit, at line breaks
split_message <- function(text, limit = 4000) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  chunks <- character(); cur <- ""
  for (l in lines) {
    if (nchar(l) > limit) l <- paste0(substr(l, 1, limit - 1), "…")
    cand <- if (nzchar(cur)) paste(cur, l, sep = "\n") else l
    if (nchar(cand) > limit) { chunks <- c(chunks, cur); cur <- l } else cur <- cand
  }
  c(chunks, cur)
}
