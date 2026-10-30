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
  x <- round(x, digits) + 0                           # + 0 turns -0 into 0
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

fmt_delta <- function(x, digits = 2) {
  x <- round(x, digits) + 0
  ifelse(is.na(x), "", ifelse(x > 0, paste0("▲", fmt_num(x, digits)),
                              ifelse(x < 0, paste0("▼", fmt_num(-x, digits)), "=")))
}

ccy_symbol <- function(ccy) {
  sym <- c(EUR = " €", USD = " $", GBP = " £", CHF = " CHF", JPY = " ¥")[ccy]
  ifelse(is.na(sym), paste0(" ", ccy), sym)
}

# Distance to the next threshold (positive = still that far away)
signal_distance <- function(sig) {
  up <- sig$direccion != "down"
  nxt <- ifelse(sig$estado == "Verde", sig$umbral_ambar, sig$umbral_rojo)
  ifelse(up, nxt - sig$valor, sig$valor - nxt)
}

section_signals <- function(r, prev, full = FALSE) {
  sig <- data.table::copy(r$signals)
  sig[, dist := signal_distance(sig)]
  n <- table(factor(sig$estado, levels = names(status_icon)))
  lines <- c(paste0("<b>Semáforo</b>  ", paste(status_icon[names(n)], n, collapse = "  ")),
             paste0("Escenario más coherente: ", html_escape(r$hint$escenario_probable)))
  show <- if (full) sig else sig[estado != "Verde"]
  show <- show[order(-match(estado, names(status_icon)))]
  if (nrow(show)) lines <- c(lines, paste0(
    status_icon[show$estado], " ", html_escape(show$senal), ": ", fmt_num(show$valor, 2),
    " (", fmt_delta(show$cambio_1d), " día · ", fmt_delta(show$cambio_1m), " mes)",
    ifelse(show$estado == "Rojo", "", paste0(" · a ", fmt_num(abs(show$dist), 2), " del ",
                                             ifelse(show$estado == "Verde", "ámbar", "rojo")))))
  near <- r$cfg$report$near_threshold
  if (!full && !is.null(near)) {
    cl <- sig[estado == "Verde" & dist <= near * pmax(abs(umbral_ambar), 1)]
    if (nrow(cl)) lines <- c(lines, paste0("Cerca del ámbar: ", paste0(
      html_escape(cl$senal), " ", fmt_num(cl$valor, 2), " (umbral ", fmt_num(cl$umbral_ambar, 2), ")",
      collapse = " · ")))
  }
  ch <- signal_changes(sig, prev)
  if (nrow(ch)) lines <- c(lines, "<b>Cambios desde el último informe</b>",
                           paste0(html_escape(ch$senal), ": ", ch$antes, " → ", ch$ahora))
  paste(lines, collapse = "\n")
}

section_markets <- function(r) {
  mk <- r$cfg$report$markets
  if (is.null(mk)) return(NULL)
  rows <- data.table::rbindlist(lapply(mk, function(m) {
    x <- r$daily[id == m$id & !is.na(value)][order(date)]
    if (nrow(x) < 2) return(NULL)
    v <- x$value[nrow(x)]; p <- x$value[nrow(x) - 1]
    data.table::data.table(label = m$label, date = x$date[nrow(x)], v = v,
                           digits = if (!is.null(m$digits)) m$digits else if (v >= 1000) 0 else 2,
                           ch = if (isTRUE(m$points)) fmt_delta(v - p) else fmt_signed(100 * (v / p - 1), 2))
  }))
  if (!nrow(rows)) return(NULL)
  # Date only for quotes that are not from the most common (latest common) session
  usual <- as.Date(names(which.max(table(format(rows$date)))))
  rows[, when := ifelse(date != usual, paste0(" (", format(date, "%d/%m"), ")"), "")]
  paste(c("<b>Mercados del día</b>",
          paste0(html_escape(rows$label), " ", mapply(fmt_num, rows$v, rows$digits), " ", rows$ch, rows$when)),
        collapse = "\n")
}

section_funds <- function(r) {
  ph <- r$portfolio[cartera == "Cartera actual"][order(date)]
  if (nrow(ph) < 2) return(NULL)
  l <- ph[.N]
  ref <- function(d) { y <- ph[date <= d]; if (nrow(y)) data.table::last(y$valor) else NA_real_ }
  ytd <- ref(as.Date(paste0(format(l$date, "%Y"), "-01-01")) - 1)
  d <- r$drift
  sim <- paste0("Año ", fmt_signed(100 * (l$valor / ytd - 1)), " · Caída desde máximo ",
                fmt_signed(l$caida_desde_max))
  if (!is.null(d) && "valor_estimado_eur" %in% names(d)) {
    # EUR snapshot drifted with the proxies; the simulation only gives year and drawdown
    v <- sum(d$valor_estimado_eur); vp <- sum(d$valor_previo_eur)
    lines <- c("<b>Cartera de fondos</b> (estimada con proxies)",
               paste0("Valor ≈ ", fmt_eur(v), " · día ", fmt_eur(v - vp, TRUE), " (",
                      fmt_signed(100 * (v / vp - 1), 2), ")",
                      if (is.na(d$desde[1])) " <i>(sin fecha as_of: importes del día anotado)</i>"
                      else paste0(" <i>(importes del ", format(d$desde[1], "%d/%m/%Y"), ")</i>")),
               paste0(sim, " <i>(simulación con tus pesos)</i>"))
  } else {
    lines <- c("<b>Cartera de fondos</b> (simulada con proxies)",
               paste0("Día ", fmt_signed(100 * (l$valor / ph[.N - 1, valor] - 1), 2), " · ", sim))
  }
  if (!is.null(d) && nrow(d)) {
    m <- d[which.max(abs(desvio_pp))]
    lines <- c(lines, paste0("Mayor desvío del objetivo: ", html_escape(m$nombre), " ",
                             fmt_signed(m$desvio_pp, 1, " pp"),
                             if (any(d$fuera_de_banda)) " (fuera de banda)" else " (dentro de banda)"))
  }
  paste(lines, collapse = "\n")
}

section_positions <- function(r, full = FALSE) {
  s <- r$positions
  if (is.null(s) || !nrow(s)) return(NULL)
  tot <- s[id == "total"]; p <- s[id != "total" & titulos > 0]
  lines <- c("<b>Posiciones</b>",
             paste0("Valor ", fmt_eur(tot$valor_eur), " · Día ", fmt_signed(tot$var_dia_pct, 2),
                    " (", fmt_eur(tot$valor_eur * tot$var_dia_pct / (100 + tot$var_dia_pct), TRUE), ")"),
             paste0("Resultado total ", fmt_eur(tot$resultado_eur, TRUE), " (",
                    fmt_signed(tot$rentab_total_pct), ")"))
  maxn <- if (full || is.null(r$cfg$report$max_positions)) nrow(p) else r$cfg$report$max_positions
  p <- p[order(-abs(var_dia_pct))][seq_len(min(maxn, .N))]
  if (nrow(p)) lines <- c(lines, paste0(
    "• ", html_escape(p$nombre), " ", fmt_num(p$precio, 2), ccy_symbol(p$divisa),
    " · día ", fmt_signed(p$var_dia_pct, 2),
    if (full) paste0(" · mes ", fmt_signed(p$var_1m_pct), " · año ", fmt_signed(p$var_anio_pct)) else "",
    " · ", fmt_eur(p$resultado_eur, TRUE), " (", fmt_signed(p$rentab_total_pct), ")",
    " · peso ", fmt_num(p$peso_pct), " %"))
  # Currency exposure, dividends this year, year-to-date versus MSCI World
  cur <- s[id != "total" & titulos > 0, .(v = sum(valor_eur)), by = divisa][order(-v)]
  if (nrow(cur)) lines <- c(lines, paste0("Divisa de cotización: ", paste0(cur$divisa, " ", fmt_num(100 * cur$v / sum(cur$v)), " %",
                                                              collapse = " · ")))
  yr <- format(max(s$fecha), "%Y")
  dv <- r$dividends
  div_y <- if (is.null(dv) || !nrow(dv)) 0 else dv[format(fecha_ex, "%Y") == yr, sum(neto_eur)]
  lines <- c(lines, paste0("Dividendos cobrados en ", yr, ": ", fmt_num(div_y, 2), " € netos"))
  wd <- r$daily[id == "world" & !is.na(value)][order(date)]
  if (nrow(wd)) {
    ref <- wd[date < as.Date(paste0(yr, "-01-01")), data.table::last(value)]
    if (length(ref)) lines <- c(lines, paste0("Año: posiciones ", fmt_signed(tot$var_anio_pct),
                                              " · MSCI World ", fmt_signed(100 * (data.table::last(wd$value) / ref - 1)),
                                              " <i>(precio, sin dividendos de las posiciones)</i>"))
  }
  paste(lines, collapse = "\n")
}

# Personal alerts: unusual daily move (GARCH), fall from the high since purchase, price
# below the average purchase price, fund portfolio outside its rebalancing band.
# Returns clave (for "nueva" tagging against the previous run) and text.
build_alerts <- function(r) {
  a <- r$cfg$report$alerts
  empty <- data.table::data.table(grupo = character(), clave = character(), texto = character())
  if (is.null(a)) return(empty)
  out <- list()
  s <- r$positions[id != "total" & titulos > 0]
  g <- r$forecast$garch$table
  if (!is.null(g) && nrow(g) && nrow(s)) {
    z <- g[id %in% s$id & abs(z_ultimo) > a$z_move][s, on = "id", nomatch = 0]
    if (nrow(z)) out$z <- data.table::data.table(
      grupo = z$nombre, clave = paste0("mov|", z$id),
      texto = paste0(fmt_signed(z$var_dia_pct, 2), " en el día, ", fmt_num(abs(z$z_ultimo), 1),
                     " desviaciones típicas (movimiento inusual)"))
  }
  if (nrow(s)) {
    dd <- s[caida_max_pct > a$drawdown]
    if (nrow(dd)) out$dd <- data.table::data.table(
      grupo = dd$nombre, clave = paste0("dd|", dd$id),
      texto = paste0(fmt_signed(-dd$caida_max_pct), " desde su máximo desde la compra"))
    if (isTRUE(a$below_cost)) {
      bc <- s[!is.na(precio_medio_eur) & valor_eur / titulos < precio_medio_eur]
      if (nrow(bc)) out$bc <- data.table::data.table(
        grupo = bc$nombre, clave = paste0("coste|", bc$id),
        texto = paste0("por debajo de su precio medio de compra (", fmt_num(bc$valor_eur / bc$titulos, 2),
                       " € frente a ", fmt_num(bc$precio_medio_eur, 2), " €)"))
    }
  }
  d <- r$drift
  if (!is.null(d) && any(d$fuera_de_banda)) {
    fb <- d[fuera_de_banda == TRUE]
    out$band <- data.table::data.table(
      grupo = fb$nombre, clave = paste0("banda|", fb$fondo),
      texto = paste0(fmt_signed(fb$desvio_pp, 1, " pp"),
                     if ("traspaso_eur" %in% names(fb))
                       paste0(", ≈ ", fmt_eur(abs(fb$traspaso_eur)),
                              ifelse(fb$traspaso_eur > 0, " por debajo del objetivo", " por encima del objetivo"))
                     else " respecto al objetivo",
                     " (banda ±", r$pf$rebalance$band_pp, " pp)",
                     if (is.na(fb$desde[1])) "" else paste0(", estimado desde ", format(fb$desde[1], "%d/%m/%Y"))))
  }
  res <- data.table::rbindlist(out)
  if (!nrow(res)) empty else res
}

# One line per position or fund; conditions not present in the previous run are marked new
section_alerts <- function(alerts, prev = NULL) {
  if (is.null(alerts) || !nrow(alerts)) return(NULL)
  a <- data.table::copy(alerts)
  a[, nueva := if (is.null(prev) || !nrow(prev)) FALSE else !clave %in% prev$clave]
  a[, texto := paste0(texto, ifelse(nueva, " <b>(nueva)</b>", ""))]
  g <- a[, .(texto = paste(texto, collapse = " · ")), by = grupo]
  paste(c("⚠️ <b>Alertas</b>", paste0("• ", html_escape(g$grupo), ": ", g$texto)),
        collapse = "\n")
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
  d <- r$drift
  if (is.null(d) || !nrow(d)) return(NULL)
  eur <- "traspaso_eur" %in% names(d)
  head <- paste0("<b>Rebalanceo</b> (pesos estimados", if (is.na(d$desde[1])) "" else
    paste0(" desde ", format(d$desde[1], "%d/%m/%Y")), ")")
  lines <- paste0("• ", html_escape(d$nombre), ": ", fmt_num(d$peso_estimado, 1), " % (objetivo ",
                  fmt_num(d$peso_objetivo, 1), " %, ", fmt_signed(d$desvio_pp, 1, " pp"), ")",
                  if (eur) paste0(" · ", fmt_eur(d$valor_estimado_eur)) else "",
                  ifelse(d$fuera_de_banda, " <b>fuera de banda</b>", ""))
  if (any(d$fuera_de_banda)) {
    tr <- d[abs(desvio_pp) > 0.05]
    lines <- c(lines, if (eur) paste0("Traspasos al objetivo, por tramo (", r$pf$rebalance$tranches, " tramos): ",
                                       paste0(html_escape(tr$nombre), ifelse(tr$traspaso_eur > 0, " recibe ", " cede "),
                                              fmt_eur(abs(tr$traspaso_eur_por_tramo)), collapse = " · "))
               else paste0("Traspasos al objetivo: ", paste0(html_escape(tr$nombre), " ",
                                                            fmt_signed(-tr$desvio_pp, 1, " pp"), collapse = " · ")))
  } else lines <- c(lines, "Todos los fondos dentro de banda")
  paste(c(head, lines), collapse = "\n")
}

# EUR value behind each GARCH series (fund portfolio needs values_eur)
garch_values <- function(r, g) {
  v <- rep(NA_real_, nrow(g))
  d <- r$drift
  if (!is.null(d) && "valor_estimado_eur" %in% names(d)) v[g$id == "Cartera actual"] <- sum(d$valor_estimado_eur)
  s <- r$positions
  if (!is.null(s) && nrow(s)) {
    v[g$id == "posiciones"] <- s[id == "total", valor_eur]
    m <- match(g$id, s$id); v[!is.na(m) & g$id != "posiciones"] <- s$valor_eur[m[!is.na(m)]]
  }
  v
}

section_garch <- function(r, full = FALSE) {
  g <- r$forecast$garch$table
  if (is.null(g) || !nrow(g)) return(NULL)
  g <- data.table::copy(g)
  g[, valor := garch_values(r, g)]
  g[, serie := data.table::fcase(
    id == "Cartera actual", "Cartera de fondos (actual)",
    id == "Cartera objetivo", "Cartera de fondos (objetivo)", default = serie)]
  world <- g[id == "ref_world", vol_anual_pct]
  spx <- g[id == "ref_spx"]
  g <- g[id != "ref_spx"]
  keep <- if (full) g$id else c("ref_world", "Cartera actual", "posiciones")
  g <- g[id %in% keep][order(match(id, c("ref_world", "Cartera actual", "Cartera objetivo", "posiciones")))]
  if (!nrow(g)) return(NULL)
  # Reference levels: the model's long-run volatility (unstable when shocks barely decay,
  # so hidden beyond a one-year half-life) and the MSCI World multiple
  g[media_vida_dias > 250, vol_largo_plazo_pct := NA_real_]
  ref_txt <- paste0(" (normal ", fmt_num(g$vol_largo_plazo_pct, 1), " %",
                    if (length(world)) ifelse(g$id == "ref_world", "",
                                              paste0(", ", fmt_num(g$vol_anual_pct / world, 1), "× MSCI World")) else "",
                    ")")
  ref_txt[is.na(g$vol_largo_plazo_pct)] <- if (length(world)) ifelse(g$id[is.na(g$vol_largo_plazo_pct)] == "ref_world", "",
    paste0(" (", fmt_num(g$vol_anual_pct[is.na(g$vol_largo_plazo_pct)] / world, 1), "× MSCI World)")) else ""
  var_txt <- ifelse(is.na(g$valor), paste0(fmt_num(g$var95_1d_pct, 2), " %"),
                    paste0("≈ ", fmt_eur(g$valor * g$var95_1d_pct / 100), " (", fmt_num(g$var95_1d_pct, 2), " %)"))
  lines <- paste0("• ", html_escape(g$serie), ": ", fmt_num(g$vol_anual_pct, 1), " % anual", ref_txt, " ",
                  fmt_delta(g$vol_anual_pct - g$vol_hace_1s_pct, 1), " en la semana (percentil ",
                  fmt_num(g$percentil_vol), ") · VaR 95 % 1 día ", var_txt,
                  " · 10 días ", fmt_num(g$var95_10d_pct, 1), " %")
  if (full) lines <- paste0(lines, "\n   ES 95 % 1 día ",
                            ifelse(is.na(g$valor), paste0(fmt_num(g$es95_1d_pct, 2), " %"),
                                   paste0("≈ ", fmt_eur(g$valor * g$es95_1d_pct / 100), " (", fmt_num(g$es95_1d_pct, 2), " %)")),
                            " · un shock se reduce a la mitad en ",
                            ifelse(g$media_vida_dias <= 250, paste0("~", fmt_num(g$media_vida_dias), " días"),
                                   "más de un año (volatilidad muy persistente)"))
  out <- c("<b>Volatilidad (GARCH)</b>", lines)
  # Implied (VIX) versus model volatility of the S&P 500
  vix <- r$daily[id == "vix" & !is.na(value)][order(date)]
  if (full && nrow(spx) && nrow(vix)) {
    gap <- data.table::last(vix$value) - spx$vol_anual_pct
    out <- c(out, paste0("VIX ", fmt_num(data.table::last(vix$value), 1), " frente a GARCH del S&P 500 ",
                         fmt_num(spx$vol_anual_pct, 1), " %: ", fmt_delta(gap, 1), " puntos, ",
                         if (gap > 6) "el mercado paga mucha más protección de la que justifican los precios (miedo anticipado)"
                         else if (gap < 0) "el mercado espera menos agitación de la que muestran los precios (complacencia)"
                         else "prima habitual (el VIX suele ir 3-4 puntos por encima)"))
  }
  rc <- r$forecast$risk
  if (full && !is.null(rc) && nrow(rc)) out <- c(out, paste0(
    "Aporte al riesgo de tus posiciones: ",
    paste0(html_escape(rc$nombre), " ", fmt_num(rc$aporte_riesgo_pct), " % (peso ", fmt_num(rc$peso_pct), " %)",
           collapse = " · ")))
  paste(out, collapse = "\n")
}

section_bootstrap <- function(r) {
  b <- r$forecast$bootstrap
  if (is.null(b)) return(NULL)
  s <- b$summary[horizonte_meses %in% c(12, 24)]
  s <- data.table::dcast(s, cartera ~ horizonte_meses, value.var = c("prob_caida", "rentab_p05"))
  s <- s[order(prob_caida_24)]                        # from lower to higher risk
  paste(c(paste0("<b>Riesgo de caída</b> (bootstrap, régimen: ", html_escape(b$regime), ")"),
          paste0("• ", html_escape(s$cartera), ": P(caída &gt; ", r$forecast_loss, " %) ",
                 fmt_num(s$prob_caida_12, 1), " % a 12 m · ", fmt_num(s$prob_caida_24, 1),
                 " % a 24 m · peor 5 % a 24 m ", fmt_signed(s$rentab_p05_24))), collapse = "\n")
}

section_logit <- function(r) {
  m <- r$forecast$logit$metrics
  if (is.null(m)) return(NULL)
  useful <- !is.na(m$auc_oos) && m$auc_oos > .6 && m$brier_oos < m$brier_clima
  paste0("<b>Alerta temprana (logit)</b>\nP(caída fuerte de la bolsa en 6 meses): ",
         fmt_num(m$prob_actual, 1), " %\nFuera de muestra: AUC ", fmt_num(m$auc_oos, 2),
         " · Brier ", fmt_num(m$brier_oos, 3), " (frecuencia histórica ", fmt_num(m$brier_clima, 3),
         ", ", m$eventos_oos, " meses con caída)",
         if (!useful) "\n<i>Sin capacidad predictiva fuera de muestra: tomar solo como referencia</i>" else "")
}

section_capm <- function(r) {
  c <- r$forecast$capm
  if (is.null(c) || !nrow(c)) return(NULL)
  paste(c("<b>CAPM frente al MSCI World</b> (36 meses)",
          paste0("• ", html_escape(c$nombre), ": beta ", fmt_num(c$beta, 2), " · alfa ",
                 fmt_signed(c$alfa_anual_pct), " anual · R² ", fmt_num(c$r2, 2))), collapse = "\n")
}

build_daily_report <- function(r, prev_signals = NULL, new_divs = NULL, alerts = NULL,
                               prev_alerts = NULL) {
  parts <- c(report_header("Monitor de riesgo · resumen diario", r),
             section_stale(r), section_alerts(alerts, prev_alerts), section_signals(r, prev_signals),
             section_markets(r), section_funds(r), section_positions(r),
             section_dividends(new_divs), section_garch(r))
  paste(parts[!is.na(parts) & nzchar(parts)], collapse = "\n\n")
}

build_weekly_report <- function(r, prev_signals = NULL, new_divs = NULL, alerts = NULL,
                                prev_alerts = NULL) {
  parts <- c(report_header("Monitor de riesgo · informe semanal", r),
             section_stale(r), section_alerts(alerts, prev_alerts), section_regime(r),
             section_signals(r, prev_signals, full = TRUE), section_markets(r),
             section_funds(r), section_scenarios(r), section_rebalance(r),
             section_positions(r, full = TRUE), section_dividends(new_divs),
             section_bootstrap(r), section_logit(r), section_capm(r), section_garch(r, full = TRUE))
  paste(parts[!is.na(parts) & nzchar(parts)], collapse = "\n\n")
}

# Should a run send now? `cron` is the GitHub schedule that fired; empty = manual or local
# run, always send. GitHub can delay or drop scheduled runs, so no particular cron is relied
# on: the first run on a report day at or after the Madrid hour sends, the others find
# `last_sent` (Madrid date of the last report of this kind) and skip.
should_send <- function(cron, kind, last_sent = NULL, now = Sys.time()) {
  if (!nzchar(cron)) return(TRUE)
  lt <- as.POSIXlt(now, tz = "Europe/Madrid")
  on_time <- if (kind == "daily") lt$wday %in% 1:5 && lt$hour >= 18 else lt$wday == 0 && lt$hour >= 10
  on_time && !identical(last_sent, format(now, "%Y-%m-%d", tz = "Europe/Madrid"))
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
