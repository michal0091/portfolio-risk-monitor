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

test_that("should_send sends once, on the first run after the Madrid hour", {
  fri <- function(t) as.POSIXct(paste("2026-10-30", t), tz = "UTC")   # winter: UTC+1
  cron <- "37 17 * * 1-5"
  expect_false(should_send(cron, "daily", NULL, fri("16:37")))         # 17:37 Madrid, too early
  expect_true(should_send(cron, "daily", NULL, fri("17:37")))
  expect_true(should_send(cron, "daily", "2026-10-29", fri("20:58")))  # late run still sends
  expect_false(should_send(cron, "daily", "2026-10-30", fri("18:37"))) # already sent today
  expect_false(should_send(cron, "daily", NULL, as.POSIXct("2026-10-31 00:30", tz = "UTC")))  # Saturday
  expect_false(should_send(cron, "daily", NULL, as.POSIXct("2026-10-29 23:30", tz = "UTC")))  # 00:30 Friday
  summer <- as.POSIXct("2026-07-01 16:37", tz = "UTC")
  expect_true(should_send(cron, "daily", NULL, summer))                # 18:37 Madrid
  sunday <- as.POSIXct("2026-11-01 09:07", tz = "UTC")                 # 10:07 Madrid
  expect_true(should_send("7 9 * * 0", "weekly", NULL, sunday))
  expect_false(should_send("7 9 * * 0", "weekly", "2026-11-01", sunday))
  expect_false(should_send("7 8 * * 0", "weekly", NULL, as.POSIXct("2026-11-01 08:07", tz = "UTC")))
  expect_true(should_send("7 9 * * 0", "monthly", NULL, sunday))
  expect_false(should_send("7 9 * * 0", "monthly", "2026-11-01", sunday))
  expect_true(should_send("", "daily", "2026-10-30", fri("03:00")))    # manual run always sends
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
              valor_eur = c(80, 500, 580), rentab_total_pct = c(-20, -3, NA),
              estrategia = c("tactical", "tactical", NA),
              caida_max_pct = c(25, 2, NA), var_dia_pct = c(-6, 1, NA)),
            forecast = list(garch = list(table = data.table::data.table(id = c("a", "b"),
                                                                         z_ultimo = c(-3.1, 0.4)))),
            drift = data.table::data.table(fondo = "x", nombre = "Fondo X", desvio_pp = 1,
                                           fuera_de_banda = FALSE, desde = as.Date(NA)))
  al <- build_alerts(r)
  expect_setequal(al$clave, c("mov|a", "dd|a", "perdida|a"))
  txt <- section_alerts(al, data.table::data.table(clave = c("dd|a", "perdida|a")))
  expect_equal(length(strsplit(txt, "\n")[[1]]), 2)               # title + one line for A
  expect_match(txt, "movimiento inusual\\) <b>\\(nueva\\)</b>")
  r2 <- r; r2$positions <- r$positions[id != "a"]
  expect_null(section_alerts(build_alerts(r2)))
  # a small loss (B, -3 %) is no alert; a core position only raises the unusual move
  r3 <- r; r3$positions <- data.table::copy(r$positions)[id == "a", estrategia := "core"]
  expect_equal(build_alerts(r3)$clave, "mov|a")
})

test_that("an active rebalance plan replaces the band alert", {
  r <- list(cfg = cfg, pf = pf, positions = data.table::data.table(id = character(), titulos = numeric()),
            drift = data.table::data.table(fondo = "x", nombre = "Fondo X", desvio_pp = 7,
                                           fuera_de_banda = TRUE, desde = as.Date(NA)))
  expect_equal(build_alerts(r)$clave, "banda|x")
  r$pf$rebalance_plan <- list(tranches = list(list(date = "2026-10-30", done = TRUE),
                                              list(date = "2026-12-15", done = FALSE)))
  expect_equal(nrow(build_alerts(r, today = as.Date("2026-11-16"))), 0)
  expect_equal(build_alerts(r, today = as.Date("2026-12-15"))$clave, "plan|2026-12-15")   # tranche due
  expect_equal(plan_label(r$pf), "plan en curso: tramo 2 de 2 previsto el 15/12/2026")
})

test_that("the logit probability is not published when the model is not useful", {
  m <- data.table::data.table(auc_oos = .39, brier_oos = .09, brier_clima = .08, prob_actual = 16.4,
                              eventos_oos = 24)
  txt <- section_logit(list(forecast = list(logit = list(metrics = m))))
  expect_match(txt, "AUC 0,39")
  expect_false(grepl("16,4", txt))
  m[, `:=`(auc_oos = .7, brier_oos = .07)]
  expect_match(section_logit(list(forecast = list(logit = list(metrics = m)))), "16,4 %")
})

test_that("the scenario section leaves out the probability-weighted row", {
  sc <- scenario_table(cfg, pf)
  txt <- section_scenarios(list(scenarios = sc, cfg = cfg))
  expect_false(grepl("Esperado", txt))
  expect_match(txt, "Supuestos revisados el")
  expect_true("Esperado" %in% sc$scenario)                       # still in scenarios.csv
})

test_that("the decision header gives action, next milestone and main change", {
  sig <- data.table::data.table(id = c("a", "b"), senal = c("A", "B"), estado = c("Rojo", "Verde"))
  r <- list(pf = pf, signals = sig,
            drift = data.table::data.table(fondo = "x", nombre = "Fondo X", fuera_de_banda = TRUE))
  txt <- section_decision(r, today = as.Date("2026-11-16"))
  expect_match(txt, "Acción:</b> rebalancear: Fondo X fuera de banda")
  expect_match(txt, "sin plan de rebalanceo en curso")
  expect_match(txt, "sin cambios en el semáforo ni alertas nuevas")
  r$pf$rebalance_plan <- list(tranches = list(list(date = "2026-10-30", done = TRUE),
                                              list(date = "2026-12-15", done = FALSE)))
  prev <- data.table::data.table(id = c("a", "b"), estado = c("Ámbar", "Verde"))
  txt <- section_decision(r, prev, today = as.Date("2026-11-16"))
  expect_match(txt, "Acción:</b> ninguna")
  expect_match(txt, "tramo 2 de 2 el 15/12/2026 \\(faltan 29 días\\)")
  expect_match(txt, "A pasa de Ámbar a Rojo")
  expect_match(section_decision(r, today = as.Date("2026-12-15")), "ejecutar el tramo 2 de 2")
  al <- data.table::data.table(clave = c("dd|a", "mov|b"))
  expect_match(section_decision(r, NULL, al, al[1], as.Date("2026-11-16")), "1 alerta nueva")
})

test_that("weekly markets show the change over seven days", {
  d <- as.Date("2026-11-02") + 0:11
  r <- list(cfg = list(report = list(markets = list(list(id = "spx", label = "S&P 500")))),
            daily = data.table::data.table(date = d, id = "spx", value = c(rep(100, 4), 110, 110, rep(120, 5), 121)))
  expect_match(section_markets(r, weekly = TRUE), "Mercados de la semana.*\\+10,00 %")   # 121 / 110
  expect_match(section_markets(r), "Mercados del día.*\\+0,83 %")
})
