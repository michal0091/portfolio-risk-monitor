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
  old <- data.table::fread(file)
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

download_all <- function(cfg) {
  raw  <- cfg$project$data_raw
  from <- cfg$project$start_date
  jobs <- c(
    lapply(cfg$yahoo, function(s) list(id = s$id, f = function() fetch_yahoo(s$ticker, s$id, from))),
    lapply(cfg$fred,  function(s) list(id = s$id, f = function() fetch_fred(s$code, s$id, from)))
  )
  out <- lapply(jobs, function(j) {
    file <- file.path(raw, paste0(j$id, ".csv"))
    if (cache_fresh(file, cfg$project$cache_days)) return(data.table::fread(file))
    res <- tryCatch(j$f(), error = function(e) {
      log_msg("WARN ", j$id, ": ", conditionMessage(e)); NULL
    })
    if (is.null(res)) {
      if (file.exists(file)) return(data.table::fread(file))  # stale cache better than nothing
      return(NULL)
    }
    res <- merge_cache(res, file)
    data.table::fwrite(res, file)
    log_msg("ok ", j$id, " (", nrow(res), " obs)")
    res
  })
  daily <- data.table::rbindlist(out, use.names = TRUE)
  daily[, date := as.Date(date)]
  daily
}
