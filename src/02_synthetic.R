# Synthetic data for an offline demo / tests ---------------------------------------
# Produces the same long format as download_all(). Crisis windows are hard-coded so
# regimes, stress windows and signals have something realistic to detect.
# Not market data: only for checking the pipeline and for a public portfolio demo.

synthetic_daily <- function(cfg, seed = 42) {
  set.seed(seed)
  dates <- seq(as.Date(cfg$project$start_date), Sys.Date(), by = "day")
  dates <- dates[!format(dates, "%u") %in% c("6", "7")]
  n <- length(dates)
  in_win <- function(a, b) dates >= as.Date(a) & dates <= as.Date(b)
  crisis <- in_win("2000-03-01", "2002-10-31") | in_win("2007-10-01", "2009-03-31") |
            in_win("2020-02-15", "2020-03-31") | in_win("2022-01-01", "2022-10-31")
  stress <- in_win("2008-09-01", "2009-03-31") | in_win("2020-03-01", "2020-04-30") |
            in_win("2026-10-01", "2026-12-31")
  infl   <- in_win("2021-06-01", "2023-06-30") | in_win("2026-03-01", "2026-12-31")

  price <- function(mu, sig, crash_mu, start = dates[1], beta = 1) {
    r <- rnorm(n, mu / 252, sig / sqrt(252)) + ifelse(crisis, beta * crash_mu / 252, 0)
    p <- 100 * exp(cumsum(r))
    p[dates < as.Date(start)] <- NA
    p
  }
  mr <- function(level, kappa, sig, spike = 0, floor = 0) {   # mean-reverting level
    x <- numeric(n); x[1] <- level
    for (i in 2:n) {
      target <- level + ifelse(stress[i], spike, 0) + ifelse(crisis[i], spike / 2, 0)
      x[i] <- max(floor, x[i - 1] + kappa * (target - x[i - 1]) + rnorm(1, 0, sig))
    }
    x
  }
  ser <- list(
    world = price(.08, .16, -.35, "2009-09-25"), europe = price(.07, .18, -.40, "2013-05-21"),
    em = price(.07, .22, -.45, "2014-06-02"), spx = price(.08, .17, -.35),
    stoxx50 = price(.06, .20, -.40), ibex = price(.05, .21, -.40), sox = price(.18, .32, -.45),
    kospi = price(.07, .24, -.50), taiex = price(.08, .22, -.45),
    nvda = price(.35, .50, -1.2, "1999-01-22"), orcl = price(.10, .30, -.60),
    crwv = price(.40, .90, -2.0, "2025-03-28"), bizd = price(.07, .15, -.25, "2013-02-12"),
    arcc = price(.07, .20, -.35, "2004-10-05"), gold = price(.07, .15, .10),
    hy_oas = mr(4.2, .02, .05, 6, 2.5), ig_oas = mr(1.2, .02, .015, 2, .5),
    eu_hy_oas = mr(4.0, .02, .05, 6, 2.2), baa10y = mr(2.3, .02, .02, 3, 1.2),
    ust10 = mr(3.8, .005, .04, -1.5, .5),
    ust2 = mr(3.0, .005, .04, -2, .1), tips10 = mr(1.2, .005, .03, 1, -1.2),
    bei10 = mr(2.2, .01, .02, -.8, .1), brent = mr(70, .01, 1.5, -25, 15),
    nfci = mr(-.4, .02, .03, 1.5, -1), stlfsi = mr(-.3, .02, .05, 3, -1.5),
    vix = mr(18, .05, .8, 25, 9), estr = mr(1.5, .002, .01, -1, -.6),
    eurusd = mr(1.15, .005, .005, -.1, .85), eurusd_fx = mr(1.15, .005, .005, -.1, .85)
  )
  # Late-2026 AI/credit stress overlay so the demo dashboard shows amber/red signals
  bump <- in_win("2026-09-01", "2026-12-31")
  ser$hy_oas[bump] <- ser$hy_oas[bump] + 1.2
  ser$baa10y[bump] <- ser$baa10y[bump] + 0.6
  ser$ust10[dates >= as.Date("2026-06-01")] <- ser$ust10[dates >= as.Date("2026-06-01")] + 1.3
  ser$brent[dates >= as.Date("2026-03-01")] <- ser$brent[dates >= as.Date("2026-03-01")] + 30

  # Monthly price indices with inflation regimes
  months <- seq(as.Date("1995-01-01"), Sys.Date(), by = "month")
  infl_m <- (months >= as.Date("2021-06-01") & months <= as.Date("2023-06-30")) |
            months >= as.Date("2026-03-01")
  cpi  <- 150 * cumprod(1 + ifelse(infl_m, .0050, .0019) + rnorm(length(months), 0, .001))
  hicp <- 70  * cumprod(1 + ifelse(infl_m, .0045, .0016) + rnorm(length(months), 0, .001))

  out <- lapply(names(ser), function(id)
    data.table::data.table(date = dates, id = id, value = ser[[id]]))
  out <- c(out, list(data.table::data.table(date = months, id = "us_cpi", value = cpi),
                     data.table::data.table(date = months, id = "ea_hicp", value = hicp)))
  data.table::rbindlist(out)[!is.na(value)]
}

# Synthetic prices, quarterly dividends and FX for tracked positions (same ids as
# download_positions). Prices start at the first lot price; no splits.
synthetic_positions <- function(positions, seed = 7) {
  set.seed(seed)
  from <- min(as.Date(unlist(lapply(positions, function(p) sapply(p$lots, `[[`, "date"))))) - 10
  dates <- seq(from, Sys.Date(), by = "day")
  dates <- dates[!format(dates, "%u") %in% c("6", "7")]
  n <- length(dates)
  out <- lapply(positions, function(p) {
    p0 <- p$lots[[1]]$price
    px <- p0 * exp(cumsum(rnorm(n, .08 / 252, .25 / sqrt(252))))
    q  <- dates[format(dates, "%m-%d") %in% c("03-15", "06-15", "09-15", "12-15")]
    dv <- if (identical(p$type, "stock")) px[match(q, dates)] * .005 else numeric()
    list(data.table::data.table(date = dates, id = paste0("px_", p$id), value = px),
         data.table::data.table(date = q[seq_along(dv)], id = rep(paste0("div_", p$id), length(dv)),
                                value = dv))
  })
  ccys <- setdiff(unique(sapply(positions, function(p) quote_ccy(p$currency))), "EUR")
  fx <- lapply(ccys, function(cc) data.table::data.table(
    date = dates, id = paste0("fx_", cc), value = 1.10 * exp(cumsum(rnorm(n, 0, .005)))))
  data.table::rbindlist(c(unlist(out, recursive = FALSE), fx))
}
