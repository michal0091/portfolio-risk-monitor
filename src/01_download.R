# Download Yahoo + FRED series with quantmod and cache them as CSV ---------------
# Output: data/raw/<id>.csv (date, id, value) and data/processed/daily.csv (long)
# The cache accumulates history: new downloads are merged into it, not overwritten
# (FRED serves only a rolling window of some series, e.g. ICE BofA OAS = 3 years).

cache_fresh <- function(file, days) {
  file.exists(file) && difftime(Sys.time(), file.mtime(file), units = "days") < days
}

# Fresh download wins on overlapping dates; older cached dates are kept
merge_cache <- function(res, file) {
  if (!file.exists(file)) return(res)
  old <- data.table::fread(file, colClasses = c(date = "character", id = "character",
                                                value = "numeric"))
  old[, date := as.Date(date)]
  res <- data.table::copy(res)[, date := as.Date(date)]
  out <- unique(data.table::rbindlist(list(res, old), use.names = TRUE), by = "date")
  out[order(date)]
}

fetch_yahoo <- function(ticker, id, from) {
  x <- quantmod::getSymbols(ticker, src = "yahoo", from = from, auto.assign = FALSE,
                            warnings = FALSE)
  xts_to_dt(quantmod::Ad(x), id)  # adjusted close (dividends reinvested)
}

fetch_fred <- function(code, id, from) {
  x <- quantmod::getSymbols(code, src = "FRED", auto.assign = FALSE, warnings = FALSE)
  dt <- xts_to_dt(x, id)
  dt[date >= as.Date(from)]
}

# FRED history plus the latest sessions from Yahoo: FRED publishes some daily series (Brent,
# VIX) with a lag of one to several days. Yahoo only adds dates after the last FRED value,
# and FRED replaces them in the cache once it publishes (fresh download wins in merge_cache).
fetch_fred_recent <- function(code, ticker, id, from) {
  dt <- fetch_fred(code, id, from)[!is.na(value)]
  y <- tryCatch(quantmod::getSymbols(ticker, src = "yahoo", from = Sys.Date() - 30,
                                     auto.assign = FALSE, warnings = FALSE),
                error = function(e) { log_msg("WARN ", id, ": no recent data from Yahoo"); NULL })
  if (is.null(y)) return(dt)
  append_recent(dt, xts_to_dt(quantmod::Cl(y), id))
}

append_recent <- function(hist, recent) {
  recent <- recent[!is.na(value)]
  if (nrow(hist)) recent <- recent[date > max(hist$date)]
  rbind(hist, recent)[order(date)]
}

# Download one series through the CSV cache. Returns NULL if it fails with no cache;
# failed ids (stale cache or missing) are collected in the "stale" attribute upstream.
# `from`: if given, a cache that starts more than a month later is refreshed even when new
# (e.g. a longer history is now needed)
cached_fetch <- function(id, f, cfg, private = FALSE, from = NULL) {
  lab <- if (private) "private series" else id   # personal tickers stay out of public CI logs
  file <- file.path(cfg$project$data_raw, paste0(id, ".csv"))
  read_cache <- function() {
    x <- data.table::fread(file, colClasses = c(date = "character", id = "character",
                                                value = "numeric"))
    x[, date := as.Date(date)]
  }
  if (cache_fresh(file, cfg$project$cache_days)) {
    x <- read_cache()
    if (is.null(from) || !nrow(x) || min(x$date) <= as.Date(from) + 31) return(x)
  }
  res <- tryCatch(f(), error = function(e) {
    log_msg("WARN ", lab, ": ", if (private) "download failed" else conditionMessage(e)); NULL
  })
  if (is.null(res)) {
    out <- if (file.exists(file)) read_cache() else NULL  # stale cache better than nothing
    return(structure(list(data = out), class = "stale_fetch"))
  }
  res <- merge_cache(res, file)
  data.table::fwrite(res, file)
  log_msg("ok ", lab, " (", nrow(res), " obs)")
  res
}

# Run a list of jobs (id + fetch function) through the cache; long table + stale ids
run_fetch_jobs <- function(jobs, cfg, private = FALSE) {
  stale <- character()
  out <- lapply(jobs, function(j) {
    res <- cached_fetch(j$id, j$f, cfg, private, j$from)
    if (inherits(res, "stale_fetch")) { stale <<- c(stale, j$id); res <- res$data }
    res
  })
  daily <- data.table::rbindlist(out, use.names = TRUE)
  if (nrow(daily)) daily[, date := as.Date(date)]
  attr(daily, "stale") <- stale
  daily
}

download_all <- function(cfg) {
  from <- cfg$project$start_date
  jobs <- c(
    lapply(cfg$yahoo, function(s) list(id = s$id, f = function() fetch_yahoo(s$ticker, s$id, from))),
    lapply(cfg$fred,  function(s) list(id = s$id, f = function()
      if (is.null(s$recent)) fetch_fred(s$code, s$id, from) else fetch_fred_recent(s$code, s$recent, s$id, from)))
  )
  run_fetch_jobs(jobs, cfg)
}

# --- Data quality -------------------------------------------------------------------
# Checks on price series (ids in price_ids): values <= 0 and isolated spikes, i.e. a daily
# move beyond `spike` that the next session undoes (a bad tick). Both are removed. The
# spike threshold is high on purpose: real V-shaped sessions exist (BDCs fell and bounced
# more than 20 % on 23-24 March 2020). A recent move beyond `jump` can be real - single
# stocks do move 20 % in a day - so it is only reported. Returns the clean table and the
# issues found (id, date, value, problema, excluido).
validate_daily <- function(daily, price_ids, jump = 0.20, spike = 0.50, recent_days = 5) {
  d <- daily[id %in% price_ids & !is.na(value)][order(id, date)]
  d[, bad := value <= 0]
  d[bad == FALSE, `:=`(r_in = value / data.table::shift(value) - 1,
                       r_out = data.table::shift(value, -1) / value - 1), by = id]
  d[, back := (1 + r_in) * (1 + r_out) - 1]             # move over the two sessions
  lim <- spike
  d[, spike := !bad & !is.na(r_in) & !is.na(r_out) & abs(r_in) > lim & sign(r_in) != sign(r_out) &
               abs(back) < abs(r_in) / 4]
  d[, big := !bad & !spike & !is.na(r_in) & abs(r_in) > jump &
             !data.table::shift(spike, fill = FALSE), by = id]   # not the way back from a spike
  last_date <- if (nrow(d)) max(d$date) else Sys.Date()
  issues <- rbind(
    d[bad == TRUE, .(id, date, value, problema = "valor no positivo", excluido = TRUE)],
    d[spike == TRUE, .(id, date, value, problema = "salto aislado que se deshace al día siguiente",
                       excluido = TRUE)],
    d[big == TRUE & date >= last_date - recent_days,
      .(id, date, value, problema = paste0("variación diaria de ", round(100 * r_in), " %: comprobar"),
        excluido = FALSE)])
  drop <- issues[excluido == TRUE, .(id, date)]
  clean <- if (nrow(drop)) daily[!drop, on = c("id", "date")] else daily
  attributes(clean)$stale <- attr(daily, "stale")
  list(daily = clean, issues = issues[order(id, date)])
}

# Last date of each series against its usual frequency (median gap of the last
# observations): a series that stopped updating is flagged even when its download "works".
# Allowed lag: 6 days for daily series, 16 for weekly, 100 for monthly (publication lag).
# max_lag: named vector of days for series with a known longer lag (max_lag_days in config).
freshness_table <- function(daily, ids = unique(daily$id), today = Sys.Date(), max_lag = NULL) {
  x <- daily[id %in% ids & !is.na(value)][order(id, date)]
  res <- x[, {
    gap <- stats::median(as.numeric(diff(utils::tail(date, 25))))
    freq <- if (.N < 2 || is.na(gap)) "desconocida" else if (gap <= 4) "diaria" else if (gap <= 10) "semanal" else "mensual"
    .(ultima_fecha = max(date), dias = as.numeric(today - max(date)), frecuencia = freq)
  }, by = id]
  res[, max_dias := c(diaria = 6, semanal = 16, mensual = 100, desconocida = 100)[frecuencia]]
  ov <- intersect(names(max_lag), res$id)
  if (length(ov)) res[match(ov, id), max_dias := as.numeric(max_lag[ov])]
  res[, desfasada := dias > max_dias]
  res[order(-desfasada, -dias)]
}
