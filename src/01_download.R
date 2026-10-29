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

# Download one series through the CSV cache. Returns NULL if it fails with no cache;
# failed ids (stale cache or missing) are collected in the "stale" attribute upstream.
cached_fetch <- function(id, f, cfg) {
  file <- file.path(cfg$project$data_raw, paste0(id, ".csv"))
  read_cache <- function() {
    x <- data.table::fread(file, colClasses = c(date = "character", id = "character",
                                                value = "numeric"))
    x[, date := as.Date(date)]
  }
  if (cache_fresh(file, cfg$project$cache_days)) return(read_cache())
  res <- tryCatch(f(), error = function(e) {
    log_msg("WARN ", id, ": ", conditionMessage(e)); NULL
  })
  if (is.null(res)) {
    out <- if (file.exists(file)) read_cache() else NULL  # stale cache better than nothing
    return(structure(list(data = out), class = "stale_fetch"))
  }
  res <- merge_cache(res, file)
  data.table::fwrite(res, file)
  log_msg("ok ", id, " (", nrow(res), " obs)")
  res
}

# Run a list of jobs (id + fetch function) through the cache; long table + stale ids
run_fetch_jobs <- function(jobs, cfg) {
  stale <- character()
  out <- lapply(jobs, function(j) {
    res <- cached_fetch(j$id, j$f, cfg)
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
    lapply(cfg$fred,  function(s) list(id = s$id, f = function() fetch_fred(s$code, s$id, from)))
  )
  run_fetch_jobs(jobs, cfg)
}
