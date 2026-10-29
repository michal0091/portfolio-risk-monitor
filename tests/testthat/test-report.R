test_that("html_escape protects Telegram HTML", {
  expect_equal(html_escape("S&P <500> & co"), "S&amp;P &lt;500&gt; &amp; co")
})

test_that("number formats use Spanish separators and signs", {
  expect_equal(fmt_num(12345.678, 2), "12.345,68")
  expect_equal(fmt_signed(c(1.234, -2, NA)), c("+1,2 %", "-2,0 %", "-"))
  expect_equal(fmt_eur(-340.4, signed = TRUE), "-340 €")
})

test_that("split_message keeps every chunk under the limit and loses no line", {
  text <- paste(sprintf("línea %03d %s", 1:300, strrep("x", 40)), collapse = "\n")
  chunks <- split_message(text, limit = 1000)
  expect_true(all(nchar(chunks) <= 1000))
  expect_equal(paste(chunks, collapse = "\n"), text)
  expect_equal(split_message("corto"), "corto")
})

test_that("scheduled_now matches the cron to Madrid time in summer and winter", {
  summer <- as.POSIXct("2026-07-01 16:35", tz = "UTC")
  winter <- as.POSIXct("2026-12-01 17:35", tz = "UTC")
  expect_true(scheduled_now("30 16 * * 1-5", 18, summer))
  expect_false(scheduled_now("30 17 * * 1-5", 18, summer))
  expect_true(scheduled_now("30 17 * * 1-5", 18, winter))
  expect_false(scheduled_now("30 16 * * 1-5", 18, winter))
  expect_true(scheduled_now("", 18, winter))                 # manual / local run
})

test_that("signal_changes reports only states that changed", {
  sig  <- data.table::data.table(id = c("a", "b"), senal = c("A", "B"), estado = c("Rojo", "Verde"))
  prev <- data.table::data.table(id = c("a", "b"), estado = c("Ámbar", "Verde"))
  ch <- signal_changes(sig, prev)
  expect_equal(nrow(ch), 1)
  expect_equal(c(ch$antes, ch$ahora), c("Ámbar", "Rojo"))
  expect_equal(nrow(signal_changes(sig, NULL)), 0)
})

test_that("new_dividends uses the seen keys, or the last days on the first run", {
  divs <- data.table::data.table(id = c("x", "x"), nombre = "X", neto_eur = 1,
                                 fecha_ex = as.Date(c("2026-01-10", "2026-10-28")))
  today <- as.Date("2026-10-29")
  expect_equal(new_dividends(divs, NULL, today = today)$fecha_ex, as.Date("2026-10-28"))
  seen <- data.table::data.table(clave = "x|2026-01-10")
  expect_equal(nrow(new_dividends(divs, seen, today = today)), 1)
  seen <- data.table::data.table(clave = dividend_key(divs))
  expect_equal(nrow(new_dividends(divs, seen, today = today)), 0)
})

test_that("send_report in dry-run writes the file and sends nothing", {
  c2 <- cfg; c2$project$output <- tempdir()
  expect_false(send_report("<b>hola</b>", "test", c2, dry_run = TRUE))
  expect_equal(readLines(file.path(tempdir(), "report_test.html")), "<b>hola</b>")
})

test_that("fmt_delta shows direction arrows", {
  expect_equal(fmt_delta(c(0.061, -0.2, 0, NA)), c("▲0,06", "▼0,20", "=", ""))
})

test_that("signal_distance measures the gap to the next threshold", {
  sig <- data.table::data.table(direccion = c("up", "up", "down"), estado = c("Verde", "Ámbar", "Verde"),
                                valor = c(4.5, 5.2, 10), umbral_ambar = c(5, 5, 8), umbral_rojo = c(6, 6, 5))
  expect_equal(signal_distance(sig), c(0.5, 0.8, 2))
})

test_that("alerts group by position and mark new conditions", {
  r <- list(cfg = cfg, pf = pf,
            positions = data.table::data.table(
              id = c("a", "b", "total"), nombre = c("A", "B", "Total"), titulos = c(10, 5, NA),
              valor_eur = c(80, 500, 580), precio_medio_eur = c(10, 90, NA),
              caida_max_pct = c(25, 2, NA), var_dia_pct = c(-6, 1, NA)),
            forecast = list(garch = list(table = data.table::data.table(id = c("a", "b"),
                                                                         z_ultimo = c(-3.1, 0.4)))),
            drift = data.table::data.table(fondo = "x", nombre = "Fondo X", desvio_pp = 1,
                                           fuera_de_banda = FALSE, desde = as.Date(NA)))
  al <- build_alerts(r)
  expect_setequal(al$clave, c("mov|a", "dd|a", "coste|a"))
  txt <- section_alerts(al, data.table::data.table(clave = c("dd|a", "coste|a")))
  expect_equal(length(strsplit(txt, "\n")[[1]]), 2)               # title + one line for A
  expect_match(txt, "movimiento inusual\\) <b>\\(nueva\\)</b>")
  r2 <- r; r2$positions <- r$positions[id != "a"]
  expect_null(section_alerts(build_alerts(r2)))
})
