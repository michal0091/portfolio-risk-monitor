# Charts (ggplot2) ------------------------------------------------------------------

theme_monitor <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold"),
                   plot.caption = ggplot2::element_text(colour = "grey40", hjust = 0))
}

plot_signals <- function(daily, cfg, years = 5, file = NULL) {
  d <- add_drawdown_series(daily)
  ids <- sapply(cfg$signals, `[[`, "id")
  lab <- setNames(sapply(cfg$signals, `[[`, "label"), ids)
  thr <- data.table::rbindlist(lapply(cfg$signals, function(s)
    data.table::data.table(id = s$id, amber = s$amber, red = s$red)))
  # Panels from most to least severe current status (config order within a status)
  st <- signal_table(na.omit(daily), cfg)[, .(id, estado)]
  st[, estado := factor(estado, levels = names(status_colours))]
  st <- st[order(-as.integer(estado), match(id, ids))]
  levs <- paste0(lab[st$id], " · ", st$estado)
  panel_of <- setNames(levs, st$id)
  x <- d[id %in% st$id & !is.na(value) & date >= max(date) - 365 * years]
  x[, panel := factor(panel_of[id], levels = levs)]
  thr <- thr[id %in% st$id][, panel := factor(panel_of[id], levels = levs)]
  last_pt <- x[order(date), .SD[.N], by = id][st, on = "id"]
  p <- ggplot2::ggplot(x, ggplot2::aes(date, value)) +
    ggplot2::geom_hline(data = thr, ggplot2::aes(yintercept = amber), colour = "#d9a400", linetype = 2) +
    ggplot2::geom_hline(data = thr, ggplot2::aes(yintercept = red), colour = "#c0392b", linetype = 2) +
    ggplot2::geom_line(colour = "grey25", linewidth = 0.4) +
    ggplot2::geom_point(data = last_pt, ggplot2::aes(colour = estado), size = 2.5) +
    ggplot2::scale_colour_manual(values = status_colours, breaks = names(status_colours),
                                 drop = FALSE) +
    ggplot2::facet_wrap(~panel, scales = "free_y", ncol = 3) +
    ggplot2::labs(title = "Señales de alerta temprana",
                  subtitle = paste("Paneles ordenados por estado actual (rojo primero).",
                                   "Línea discontinua: umbral ámbar (amarillo) y rojo"),
                  x = NULL, y = NULL, colour = "Estado actual",
                  caption = "Fuente: Yahoo Finance y FRED vía quantmod") +
    theme_monitor() + ggplot2::theme(legend.position = "bottom")
  if (!is.null(file)) ggplot2::ggsave(file, p, width = 11, height = 8, dpi = 120, bg = "white")
  p
}

# Legends run from lower to higher risk, left to right; colours darken/redden with risk.
# Tense credit ranks above high inflation: it is what drove the 2000-02 and 2008 drawdowns.
regime_colours <- c("Inflación baja · Crédito relajado"   = "#9ecae1",
                    "Inflación alta · Crédito relajado"   = "#fee391",
                    "Inflación baja · Crédito tensionado" = "#fdae6b",
                    "Inflación alta · Crédito tensionado" = "#e34a33")
status_colours <- c("Verde" = "#2e9e44", "Ámbar" = "#d9a400", "Rojo" = "#c0392b")

plot_regimes <- function(w, file = NULL) {
  x <- w[!is.na(regime), .(date = zoo::as.Date(m_y), end = zoo::as.Date(m_y + 1/12),
                            regime, spx = spx)][!is.na(spx)]
  x[, `:=`(lo = min(spx), hi = max(spx))]
  p <- ggplot2::ggplot(x) +
    ggplot2::geom_rect(ggplot2::aes(xmin = date, xmax = end, ymin = lo, ymax = hi,
                                    fill = regime), alpha = .35) +
    ggplot2::geom_line(ggplot2::aes(date, spx), linewidth = .4) +
    ggplot2::scale_y_log10() +
    ggplot2::scale_fill_manual(values = regime_colours, breaks = names(regime_colours)) +
    ggplot2::labs(title = "Regímenes macro y S&P 500", fill = NULL, x = NULL,
                  y = "S&P 500 (escala log)") +
    theme_monitor() + ggplot2::theme(legend.position = "bottom")
  if (!is.null(file)) ggplot2::ggsave(file, p, width = 11, height = 5, dpi = 120, bg = "white")
  p
}

plot_scenarios <- function(sc, cfg, file = NULL) {
  s <- sc[scenario != "Esperado"]
  # Scenarios from mildest to worst outcome for the target portfolio
  ord <- s[order(-conjunto_objetivo), scenario]
  nm  <- cfg$scenarios$names
  lab <- sprintf("%s. %s\n(%s %%)", ord, unlist(nm[ord]), round(100 * s[match(ord, scenario), prob]))
  x <- data.table::melt(s, id.vars = c("scenario", "prob"),
                        variable.name = "cartera", value.name = "ret")
  x[, scenario := factor(scenario, levels = ord, labels = lab)]
  x[, cartera := factor(cartera, levels = c("plan_pensiones", "conjunto_objetivo",
                                            "fondos_objetivo", "fondos_actual"),
                        labels = c("Plan de pensiones", "Conjunto objetivo",
                                   "Fondos objetivo", "Fondos actual"))]
  p <- ggplot2::ggplot(x, ggplot2::aes(scenario, ret, fill = cartera)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(.8), width = .75) +
    ggplot2::geom_hline(yintercept = -15, linetype = 2, colour = "#c0392b") +
    ggplot2::annotate("text", x = .6, y = -15, label = "Tolerancia -15 %", vjust = -.5,
                      hjust = 0, size = 3, colour = "#c0392b") +
    ggplot2::scale_fill_manual(values = c("#a1dab4", "#41b6c4", "#2c7fb8", "#253494")) +
    ggplot2::labs(title = "Pérdida estimada por escenario (12-24 meses)",
                  subtitle = "Sin fondo de emergencia; supuestos en cfg/config.yaml. De menor a mayor riesgo",
                  x = NULL, y = "%", fill = NULL) +
    theme_monitor() + ggplot2::theme(legend.position = "bottom")
  if (!is.null(file)) ggplot2::ggsave(file, p, width = 9, height = 5, dpi = 120, bg = "white")
  p
}
