suppressPackageStartupMessages({
  library(httr2); library(rvest); library(jsonlite); library(dplyr)
  library(stringr); library(purrr); library(readr); library(tibble)
})

UA <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0 Safari/537.36"

# Columns every parser returns (missing ones are filled with NA).
LISTING_COLS <- c("source", "id", "url", "title", "address", "postcode", "city",
                  "rooms", "m2", "rent", "aconto", "status", "available_from")

req_get <- function(url, ...) {
  request(url) |>
    req_user_agent(UA) |>
    req_headers(`Accept-Language` = "da-DK,da;q=0.9,en;q=0.8", ...) |>
    req_timeout(40) |>
    req_retry(max_tries = 3, backoff = ~ 5) |>
    req_perform()
}
get_html <- function(url, ...) read_html(resp_body_string(req_get(url, ...)))
get_json <- function(url, simplifyVector = TRUE, ...)
  resp_body_json(req_get(url, Accept = "application/json", ...), simplifyVector = simplifyVector)

# "12.837 kr." -> 12837 ; "12.837,50" -> 12837.5 ; "79 m²" -> 79 ; "3½" -> 3.5
dk_num <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "½", ".5")
  x <- str_extract(x, "\\d[\\d.\\s]*(,\\d+)?(\\.5)?")
  x <- str_replace_all(x, "\\s", "")
  half <- str_detect(x, "\\.5$") & !str_detect(x, "\\.\\d{3}")
  x <- ifelse(half, x, str_replace_all(x, "\\.(?=\\d{3}(\\D|$))", ""))
  suppressWarnings(as.numeric(str_replace(x, ",", ".")))
}
postcode_of <- function(x) str_extract(as.character(x), "\\b[1-9]\\d{3}(?=\\s+[A-ZÆØÅa-zæøå])")

# Representative postcode for a district name when a site gives no postcode
# ("København K" -> "1100"). Only used for filtering; marked in the log.
city_postcode <- function(city) {
  c <- str_to_lower(str_squish(str_replace_all(as.character(city), "-", " ")))
  c <- str_replace_all(c, c("oe" = "ø", "aa" = "å", "ae" = "æ"))
  case_when(
    str_detect(c, "k(ø|o)benhavn k\\b|indre by") ~ "1100",
    str_detect(c, "k(ø|o)benhavn sv\\b|sydhavn|sluseholmen") ~ "2450",
    str_detect(c, "k(ø|o)benhavn nv\\b") ~ "2400",
    str_detect(c, "k(ø|o)benhavn v\\b|vesterbro") ~ "1600",
    str_detect(c, "k(ø|o)benhavn n\\b|nørrebro") ~ "2200",
    str_detect(c, "k(ø|o)benhavn ø\\b|østerbro") ~ "2100",
    str_detect(c, "nordhavn") ~ "2150",
    str_detect(c, "k(ø|o)benhavn s\\b|amager|ørestad|islands brygge") ~ "2300",
    str_detect(c, "valby") ~ "2500",
    str_detect(c, "frederiksberg") ~ "2000",
    TRUE ~ NA_character_)
}

as_listings <- function(df, source) {
  df <- as_tibble(df)
  for (c in setdiff(LISTING_COLS, names(df))) df[[c]] <- NA
  df |>
    mutate(source = source,
           id = paste0(source, ":", id),
           postcode = coalesce(as.character(postcode), postcode_of(address), postcode_of(title),
                               city_postcode(city), city_postcode(address)),
           across(c(rooms, m2, rent, aconto), ~ suppressWarnings(as.numeric(.x))),
           across(c(title, address, city, status, url, available_from), ~ str_squish(as.character(.x)))) |>
    select(all_of(LISTING_COLS)) |>
    distinct(id, .keep_all = TRUE)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# Pull a value from a detail page's text by label, e.g. aconto
text_after <- function(txt, label_regex) {
  m <- str_match(txt, regex(paste0(label_regex, "[^\\d]{0,40}(\\d[\\d.]*(,\\d+)?)"), ignore_case = TRUE))
  dk_num(m[, 2])
}
