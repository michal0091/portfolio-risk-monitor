# Utilities shared by all modules ------------------------------------------------

log_msg <- function(...) message(format(Sys.time(), "[%H:%M:%S] "), ...)

read_cfg <- function(path = "cfg/config.yaml") yaml::read_yaml(path, fileEncoding = "UTF-8")

# Personal YAML (git-ignored) with fallback to the public example
read_personal <- function(path, fallback) {
  if (!file.exists(path)) {
    log_msg(path, " not found, using ", fallback)
    path <- fallback
  }
  yaml::read_yaml(path, fileEncoding = "UTF-8")
}

read_portfolio <- function(path = "cfg/portfolio.yaml",
                           fallback = "cfg/portfolio.example.yaml") {
  normalize_portfolio(read_personal(path, fallback))
}

# When every fund has a value in values_eur, those amounts are the source of truth:
# weights_current is derived from them so all modules use the same snapshot.
normalize_portfolio <- function(pf) {
  ids <- names(pf$weights_target)
  v <- pf$values_eur[ids]
  pf$values_based <- length(v) == length(ids) && !any(sapply(v, is.null))
  if (pf$values_based) {
    vals <- unlist(v)
    pf$weights_current <- as.list(100 * vals / sum(vals))
  }
  pf
}

read_positions <- function(path = "cfg/positions.yaml",
                           fallback = "cfg/positions.example.yaml") {
  read_personal(path, fallback)$positions
}

ensure_dirs <- function(cfg) {
  for (d in c(cfg$project$data_raw, cfg$project$data_processed, cfg$project$output)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
}

# xts -> long data.table (date, id, value)
xts_to_dt <- function(x, id) {
  data.table::data.table(date = as.Date(zoo::index(x)), id = id,
                         value = as.numeric(x[, 1]))
}

# Daily long table -> monthly last observation, with zoo::yearmon key `m_y`
to_monthly <- function(dt) {
  dt <- data.table::copy(dt)[!is.na(value)]
  dt[, m_y := zoo::as.yearmon(date)]
  dt[order(date), .(date = data.table::last(date), value = data.table::last(value)),
     by = .(id, m_y)]
}

# Drawdown (%) from running maximum, positive numbers = loss
drawdown_pct <- function(x) {
  peak <- cummax(ifelse(is.na(x), -Inf, x))
  100 * (1 - x / peak)
}

# Percentile of last value within its own history
last_percentile <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  100 * mean(x <= data.table::last(x))
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "-", formatC(x, format = "f", digits = digits, decimal.mark = ","))
}
