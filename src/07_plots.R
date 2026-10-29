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
  x <- d[id %in% ids & !is.na(value) & date >= max(date) - 365 * years]
  x[, panel := factor(lab[id], levels = lab)]
  thr[, panel := factor(lab[id], levels = lab)]
  p <- ggplot2::ggplot(x, ggplot2::aes(date, value)) +
    ggplot2::geom_hline(data = thr, ggplot2::aes(yintercept = amber), colour = "#d9a400", linetype = 2) +
    ggplot2::geom_hline(data = thr, ggplot2::aes(yintercept = red), colour = "#c0392b", linetype = 2) +
    ggplot2::geom_line(colour = "grey25", linewidth = 0.4) +
    ggplot2::facet_wrap(~panel, scales = "free_y", ncol = 3) +
    ggplot2::labs(title = "Señales de alerta temprana",
                  subtitle = "Línea discontinua: umbral ámbar (amarillo) y rojo",
                  x = NULL, y = NULL, caption = "Fuente: Yahoo Finance y FRED vía quantmod") +
    theme_monitor()
  if (!is.null(file)) ggplot2::ggsave(file, p, width = 11, height = 8, dpi = 120, bg = "white")
  p
}

plot_regimes <- function(w, file = NULL) {
  x <- w[!is.na(regime), .(date = zoo::as.Date(m_y), end = zoo::as.Date(m_y + 1/12),
                            regime, spx = spx)][!is.na(spx)]
  x[, `:=`(lo = min(spx), hi = max(spx))]
  p <- ggplot2::ggplot(x) +
    ggplot2::geom_rect(ggplot2::aes(xmin = date, xmax = end, ymin = lo, ymax = hi,
                                    fill = regime), alpha = .35) +
    ggplot2::geom_line(ggplot2::aes(date, spx), linewidth = .4) +
    ggplot2::scale_y_log10() +
    ggplot2::scale_fill_manual(values = c("Inflación baja · Crédito relajado" = "#9ecae1",
                                          "Inflación baja · Crédito tensionado" = "#fdae6b",
                                          "Inflación alta · Crédito relajado" = "#a1d99b",
                                          "Inflación alta · Crédito tensionado" = "#fb6a4a")) +
    ggplot2::labs(title = "Regímenes macro y S&P 500", fill = NULL, x = NULL,
                  y = "S&P 500 (escala log)") +
    theme_monitor() + ggplot2::theme(legend.position = "bottom")
  if (!is.null(file)) ggplot2::ggsave(file, p, width = 11, height = 5, dpi = 120, bg = "white")
  p
}

plot_scenarios <- function(sc, file = NULL) {
  x <- data.table::melt(sc[scenario != "Esperado"], id.vars = c("scenario", "prob"),
                        variable.name = "cartera", value.name = "ret")
  x[, cartera := factor(cartera, levels = c("fondos_actual", "fondos_objetivo",
                                            "plan_pensiones", "conjunto_objetivo"),
                        labels = c("Fondos actual", "Fondos objetivo",
                                   "Plan de pensiones", "Conjunto objetivo"))]
  p <- ggplot2::ggplot(x, ggplot2::aes(scenario, ret, fill = cartera)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(.8), width = .75) +
    ggplot2::geom_hline(yintercept = -15, linetype = 2, colour = "#c0392b") +
    ggplot2::annotate("text", x = .6, y = -15, label = "Tolerancia -15 %", vjust = -.5,
                      hjust = 0, size = 3, colour = "#c0392b") +
    ggplot2::scale_fill_manual(values = c("grey70", "#2c7fb8", "#7fcdbb", "#253494")) +
    ggplot2::labs(title = "Pérdida estimada por escenario (12-24 meses)",
                  subtitle = "Cartera de fondos sin fondo de emergencia; supuestos en cfg/config.yaml",
                  x = NULL, y = "%", fill = NULL) +
    theme_monitor() + ggplot2::theme(legend.position = "bottom")
  if (!is.null(file)) ggplot2::ggsave(file, p, width = 9, height = 5, dpi = 120, bg = "white")
  p
}
