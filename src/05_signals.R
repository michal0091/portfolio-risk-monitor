# Early-warning dashboard: latest value, change, percentile and traffic light -----

add_drawdown_series <- function(daily) {
  dd_ids <- c(sox = "sox_dd", kospi = "kospi_dd", bizd = "bizd_dd", crwv = "crwv_dd")
  dd <- daily[id %in% names(dd_ids)][order(id, date)]
  dd[, value := drawdown_pct(value), by = id]
  dd[, id := dd_ids[id]]
  rbind(daily, dd)
}

signal_table <- function(daily, cfg) {
  d <- add_drawdown_series(daily)
  rows <- lapply(cfg$signals, function(s) {
    x <- d[id == s$id][order(date)]
    if (!nrow(x)) return(NULL)
    last_v <- data.table::last(x$value); last_d <- data.table::last(x$date)
    val_at <- function(days) {
      y <- x[date <= last_d - days]
      if (nrow(y)) data.table::last(y$value) else NA_real_
    }
    status <- if (s$direction == "up") {
      if (last_v >= s$red) "Rojo" else if (last_v >= s$amber) "Ámbar" else "Verde"
    } else {
      if (last_v <= s$red) "Rojo" else if (last_v <= s$amber) "Ámbar" else "Verde"
    }
    data.table::data.table(
      id = s$id, senal = s$label, fecha = last_d, valor = last_v,
      cambio_1m = last_v - val_at(30), cambio_3m = last_v - val_at(91),
      percentil_hist = last_percentile(x$value), umbral_ambar = s$amber,
      umbral_rojo = s$red, estado = status)
  })
  data.table::rbindlist(rows)
}

# Rough scenario read-out from the signals (heuristic, not a model)
scenario_hint <- function(sig) {
  st <- setNames(sig$estado, sig$id)
  red <- function(ids) sum(st[intersect(ids, names(st))] == "Rojo")
  amb <- function(ids) sum(st[intersect(ids, names(st))] %in% c("Rojo", "Ámbar"))
  credit <- amb(c("hy_oas", "ig_oas", "eu_hy_oas", "bizd_dd", "crwv_dd"))
  rates  <- amb(c("ust10", "tips10", "brent"))
  ai     <- amb(c("sox_dd", "kospi_dd", "crwv_dd"))
  panic  <- red(c("vix", "stlfsi", "hy_oas"))
  hint <- if (panic >= 2 && rates == 0) "C (crash deflacionario): pánico sin presión de tipos"
    else if (credit >= 2 && rates >= 2) "B (estanflación + evento de crédito)"
    else if (ai >= 1 || credit >= 1) "A (desinflado ordenado del ciclo IA)"
    else "D (aterrizaje suave) o sin señales"
  data.table::data.table(senales_credito = credit, senales_tipos_inflacion = rates,
                         senales_ia = ai, senales_panico = panic, escenario_probable = hint)
}
