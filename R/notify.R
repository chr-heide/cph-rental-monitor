# Push notifications via ntfy (https://ntfy.sh). Anyone who knows the topic can read or post
# to it, so it should come from the NTFY_TOPIC repository secret; config/ntfy_topic.txt is
# only a fallback for when no secret is set.
ntfy_post <- function(body) {
  topic <- Sys.getenv("NTFY_TOPIC")
  if (!nzchar(topic) && file.exists("config/ntfy_topic.txt")) topic <- str_trim(readLines("config/ntfy_topic.txt", 1))
  if (!nzchar(topic)) { message("NTFY_TOPIC not set; would send: ", body$title); return(FALSE) }
  body$topic <- topic
  # TRUE only when ntfy accepted the message (returns its id); failed pushes are retried next run
  ok <- tryCatch({
    r <- request(Sys.getenv("NTFY_SERVER", "https://ntfy.sh")) |>
      req_body_json(body, auto_unbox = TRUE) |> req_timeout(20) |> req_retry(max_tries = 2) |> req_perform()
    !is.null(resp_body_json(r)$id)
  }, error = function(e) { message("ntfy failed: ", conditionMessage(e)); FALSE })
  invisible(ok)
}

fmt_kr <- function(x) ifelse(is.na(x), "?", format(round(x), big.mark = ".", decimal.mark = ","))

notify_listing <- function(r) {
  rent_txt <- if (is.na(r$rent)) "leje ikke oplyst" else if (isTRUE(r$aconto_assumed))
    paste0(fmt_kr(r$rent), " kr + aconto ukendt") else
    paste0(fmt_kr(r$rent_total), " kr inkl. aconto")
  ntfy_post(list(
    title    = paste0(coalesce(r$address, r$title, "Ny bolig"), if (!is.na(r$postcode)) paste0(" (", r$postcode, ")") else ""),
    message  = paste0(fmt_kr(r$m2), " m² · ", fmt_kr(r$rooms), " vær · ", rent_txt,
                      if (!is.na(r$available_from)) paste0(" · fra ", r$available_from) else "",
                      "\n", r$source, if (!is.na(r$title) && !identical(r$title, r$address)) paste0(": ", r$title) else ""),
    click    = r$url %||% "",
    tags     = list("house"),
    priority = 4))
}

notify_text <- function(text, title = "Rental monitor") ntfy_post(list(title = title, message = text, tags = list("warning")))
