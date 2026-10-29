# Telegram Bot API (sendMessage / sendPhoto) via httr2 ----------------------------------
# Credentials come from TELEGRAM_BOT_TOKEN and TELEGRAM_CHAT_ID. Without them (or with
# dry_run = TRUE) nothing is sent: the report is written to output/report_<kind>.html.
# Errors never print the request URL, which contains the token.

tg_credentials <- function() {
  token <- Sys.getenv("TELEGRAM_BOT_TOKEN"); chat <- Sys.getenv("TELEGRAM_CHAT_ID")
  if (!nzchar(token) || !nzchar(chat)) return(NULL)
  list(token = token, chat = chat)
}

tg_request <- function(cred, method) {
  httr2::request(paste0("https://api.telegram.org/bot", cred$token, "/", method)) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(resp) FALSE)
}

tg_check <- function(resp, what) {
  if (httr2::resp_status(resp) >= 300) {
    desc <- tryCatch(httr2::resp_body_json(resp)$description, error = function(e) "")
    stop("Telegram ", what, " failed: HTTP ", httr2::resp_status(resp), " ", desc, call. = FALSE)
  }
  invisible(TRUE)
}

tg_send_message <- function(text, cred) {
  for (chunk in split_message(text)) {
    resp <- tg_request(cred, "sendMessage") |>
      httr2::req_body_json(list(chat_id = cred$chat, text = chunk, parse_mode = "HTML",
                                disable_web_page_preview = TRUE)) |>
      httr2::req_perform()
    tg_check(resp, "sendMessage")
  }
  invisible(TRUE)
}

tg_send_photo <- function(file, caption, cred) {
  resp <- tg_request(cred, "sendPhoto") |>
    httr2::req_body_multipart(chat_id = cred$chat, caption = caption,
                              photo = curl::form_file(file)) |>
    httr2::req_perform()
  tg_check(resp, "sendPhoto")
}

# Send (or save, in dry-run mode) a text report plus optional charts (named: caption)
send_report <- function(text, kind, cfg, photos = character(), dry_run = FALSE) {
  cred <- if (dry_run) NULL else tg_credentials()
  if (is.null(cred)) {
    file <- file.path(cfg$project$output, paste0("report_", kind, ".html"))
    if (length(photos)) text <- c(text, "", paste0("[gráfico] ", names(photos), ": ", photos))
    writeLines(text, file, useBytes = TRUE)
    log_msg("Dry run: report saved to ", file, " (not sent)")
    return(invisible(FALSE))
  }
  tg_send_message(text, cred)
  for (i in seq_along(photos)) {
    if (file.exists(photos[[i]])) tg_send_photo(photos[[i]], names(photos)[i], cred)
  }
  log_msg("Report sent to Telegram (", kind, ")")
  invisible(TRUE)
}
