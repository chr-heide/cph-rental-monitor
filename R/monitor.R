# Scrape every enabled site, log all listings, push alerts for matches not alerted before.
# Run from repo root:  Rscript R/monitor.R               (normal run)
#                      Rscript R/monitor.R --dry-run     (no alerts, no state writes)
#                      Rscript R/monitor.R --no-alerts   (writes the log, sends nothing)
#                      Rscript R/monitor.R --only=kereby,taurus
source("R/utils.R")
for (f in list.files("R/sites", full.names = TRUE, pattern = "\\.R$")) source(f)
source("R/notify.R")
source("R/geocode.R")
library(yaml)
if (nzchar(Sys.getenv("MONITOR_MOCK"))) source(Sys.getenv("MONITOR_MOCK"))  # tests: serve saved pages

args    <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
no_alerts <- dry_run || "--no-alerts" %in% args
only_arg <- grep("^--only=", args, value = TRUE)
only    <- if (length(only_arg)) str_split(str_remove(only_arg[1], "^--only="), ",")[[1]] else character(0)
now     <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "Europe/Copenhagen")
MAX_DETAIL <- 60   # detail pages fetched per run (all sites together)
MAX_PUSH   <- 25   # phone alerts per run; the rest go out next run

sites <- read_csv("config/sites.csv", show_col_types = FALSE, comment = "#",
                  col_types = cols(.default = "c", enabled = "l"))
sites <- filter(sites, enabled)
if (length(only) && nzchar(only[1])) sites <- filter(sites, name %in% only)
filt <- read_yaml("config/filters.yml")

# ---- scrape -------------------------------------------------------------------
results <- list(); status <- list()
for (i in seq_len(nrow(sites))) {
  s <- sites[i, ]; t0 <- Sys.time()
  raw <- tryCatch(get(paste0("scrape_", s$parser))(s$url, s$param), error = function(e) e)
  # parsers that only read the newest pages set attr(x, "partial"): listings missing from them aren't "gone"
  partial <- !inherits(raw, "error") && isTRUE(attr(raw, "partial"))
  res <- if (inherits(raw, "error")) raw else tryCatch(as_listings(raw, s$name), error = function(e) e)
  # 0 listings normally means a broken parser; parsers that verified the page is a real
  # "no vacancies right now" page say so with attr(x, "empty_ok")
  empty_ok <- !inherits(raw, "error") && isTRUE(attr(raw, "empty_ok"))
  ok <- !inherits(res, "error") && (nrow(res) > 0 || empty_ok)
  if (ok) results[[s$name]] <- res
  err <- if (inherits(res, "error")) conditionMessage(res) else if (!ok) "0 listings parsed" else NA_character_
  status[[s$name]] <- tibble(site = s$name, run_at = now, ok = ok, partial = partial, n = if (ok) nrow(res) else 0L,
                             error = str_trunc(str_squish(err), 200),
                             secs = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1))
  message(sprintf("%-20s %s %4s  %s", s$name, if (ok) "ok " else "ERR", if (ok) nrow(res) else "-", coalesce(err, "")))
  Sys.sleep(1)  # polite gap between sites
}
status <- bind_rows(status)
cur <- bind_rows(results)
if (!nrow(cur)) cur <- as_listings(tibble(id = character()), "none")

# ---- carry over what earlier runs learned (detail pages, alerts) ------------------
log_path <- "data/listings.csv"
old <- if (file.exists(log_path)) read_csv(log_path, col_types = cols(.default = "c")) else tibble(id = character())
for (c in c("source", "rooms", "m2", "rent", "aconto", "postcode", "available_from",
            "first_seen", "alerted", "detail_done", "gone_at"))
  if (!c %in% names(old)) old[[c]] <- NA_character_
num_cols <- c("rooms", "m2", "rent", "aconto")
for (c in c("lat", "lon")) if (!c %in% names(old)) old[[c]] <- NA_character_
num_cols <- c(num_cols, "lat", "lon")
prev <- old |> select(id, all_of(num_cols), postcode, available_from, first_seen, alerted, detail_done) |>
  mutate(across(all_of(num_cols), as.numeric)) |> rename_with(~ paste0(.x, "_old"), -id)
cur <- cur |> left_join(prev, by = "id") |>
  mutate(rooms = coalesce(rooms, rooms_old), m2 = coalesce(m2, m2_old), rent = coalesce(rent, rent_old),
         lat = coalesce(lat, lat_old), lon = coalesce(lon, lon_old),
         aconto = coalesce(aconto, aconto_old), postcode = coalesce(postcode, postcode_old),
         available_from = coalesce(available_from, available_from_old),
         first_seen = coalesce(first_seen_old, now), alerted = alerted_old, detail_done = detail_done_old) |>
  select(-ends_with("_old"))

# ---- scoring ----------------------------------------------------------------------
in_ranges <- function(pc, specs) {
  pc <- suppressWarnings(as.integer(pc)); hit <- rep(FALSE, length(pc))
  for (sp in specs) {
    r <- as.integer(str_split(sp, "-")[[1]]); if (length(r) == 1) r <- c(r, r)
    hit <- hit | (!is.na(pc) & pc >= r[1] & pc <= r[2])
  }
  hit
}
excl_re <- if (length(filt$exclude_keywords)) paste0("(", paste(filt$exclude_keywords, collapse = ")|("), ")") else "$^"
unavailable_re <- "udlejet|reserveret|reserved|lukket|optaget|rented|solgt|venteliste"
in_box <- function(pc, lat, lon) {   # TRUE unless the postcode has a box and the point is outside it
  out <- rep(TRUE, length(pc))
  for (p in names(filt$area_boxes)) {
    b <- filt$area_boxes[[p]]; sel <- !is.na(pc) & pc == p
    inside <- !is.na(lat) & lat >= b$lat[1] & lat <= b$lat[2] & lon >= b$lon[1] & lon <= b$lon[2]
    out[sel] <- inside[sel] | (is.na(lat[sel]) & isTRUE(filt$alert_on_unknown))
  }
  out
}
score <- function(df) {
  unk <- isTRUE(filt$alert_on_unknown)
  df |> mutate(
    aconto_assumed = is.na(aconto) & !is.na(rent),
    rent_total     = rent + coalesce(aconto, 0),
    rent_check     = if_else(aconto_assumed, rent + filt$assumed_aconto_if_missing, rent_total),
    ok_area  = in_ranges(postcode, filt$postcodes) & in_box(postcode, lat, lon),
    ok_m2    = if_else(is.na(m2), unk, m2 >= filt$min_m2),
    ok_rooms = if_else(is.na(rooms), unk, rooms >= filt$min_rooms),
    ok_rent  = if_else(is.na(rent_check), unk, rent_check <= filt$max_rent_total),
    ok_avail = !str_detect(coalesce(str_to_lower(status), ""), unavailable_re),
    ok_kw    = !str_detect(str_to_lower(paste(title, address)), excl_re),
    ok_info  = !is.na(rent) | !is.na(m2),   # never alert on a listing we know nothing about
    match    = ok_area & ok_m2 & ok_rooms & ok_rent & ok_avail & ok_kw & ok_info)
}

# ---- detail pages: fill aconto / m2 / postcode for listings that could match --------
has_detail <- function(src) !is.null(get0(paste0("detail_", sites$parser[sites$name == src][1])))
cand <- score(cur) |>
  filter(is.na(detail_done), ok_area | is.na(postcode), ok_m2, ok_rooms, ok_avail,
         is.na(rent_check) | rent_check <= filt$max_rent_total + 3000, !is.na(url)) |>
  filter(map_lgl(source, has_detail)) |>
  arrange(desc(first_seen)) |> head(MAX_DETAIL)
for (i in seq_len(nrow(cand))) {
  fn <- get(paste0("detail_", sites$parser[sites$name == cand$source[i]][1]))
  d <- tryCatch(fn(cand$url[i]), error = function(e) NULL)
  j <- which(cur$id == cand$id[i])
  if (!is.null(d)) for (f in intersect(names(d), c("aconto", "m2", "rooms", "rent", "postcode", "available_from", "address"))) {
    v <- d[[f]]
    # detail page fills gaps; for aconto it wins (list pages rarely show it)
    if (length(v) == 1 && !is.na(v) && (is.na(cur[[f]][j]) || f == "aconto")) cur[[f]][j] <- v
  }
  cur$detail_done[j] <- now
  Sys.sleep(0.5)
}

# ---- coordinates for possible matches in box-restricted postcodes (e.g. 2300 -> Amagerbro) --
geo_need <- score(cur) |> filter(postcode %in% names(filt$area_boxes), is.na(lat), ok_m2, ok_rooms, ok_rent, ok_avail, ok_kw)
if (nrow(geo_need)) {
  geo <- geocode_missing(select(geo_need, id, address, postcode, lat, lon))
  i <- match(geo$id, cur$id); cur$lat[i] <- geo$lat; cur$lon[i] <- geo$lon
}

scored <- score(cur)

# ---- alerts -------------------------------------------------------------------------
# wait for the detail page (aconto, rooms, exact postcode) when the site has one and it hasn't been read yet
detail_pending <- map_lgl(scored$source, has_detail) & is.na(scored$detail_done)
to_alert <- scored |> filter(match, is.na(alerted), !detail_pending) |> arrange(rent_check)
pushed <- head(to_alert, MAX_PUSH)
if (!no_alerts && nrow(pushed)) {
  delivered <- map_lgl(seq_len(nrow(pushed)), ~ isTRUE(notify_listing(pushed[.x, ])))
  if (nrow(to_alert) > MAX_PUSH)
    notify_text(sprintf("%d more matches queued for the next run(s).", nrow(to_alert) - MAX_PUSH), title = "More matches")
  scored$alerted[scored$id %in% pushed$id[delivered]] <- now
  pushed <- pushed[delivered, ]
}

# ---- site health: alert once when a site has failed for 2 hours straight ----------------
# (time-based, so short outages on a landlord's server don't alert at any run frequency;
#  a "working again" note follows if that site later recovers)
FAIL_ALERT_HOURS <- 2
stat_path <- "data/site_status.csv"
stat_old <- if (file.exists(stat_path)) read_csv(stat_path, col_types = cols(.default = "c")) else tibble()
stat_new <- bind_rows(stat_old, mutate(status, across(everything(), as.character))) |>
  group_by(site) |> slice_tail(n = 60) |> ungroup()
hours_between <- function(a, b) as.numeric(difftime(as.POSIXct(b, format = "%Y-%m-%dT%H:%M:%S"),
                                                     as.POSIXct(a, format = "%Y-%m-%dT%H:%M:%S"), units = "hours"))
if (!no_alerts) for (s in status$site) {
  h <- stat_new |> filter(site == s)
  n <- nrow(h); if (n < 2) next
  ok <- h$ok == "TRUE"
  # start of the current failure streak (or of the streak that just ended)
  streak_start <- function(upto) { k <- upto; while (k > 1 && !ok[k - 1]) k <- k - 1; k }
  if (!ok[n]) {
    k <- streak_start(n)
    now_h  <- hours_between(h$run_at[k], h$run_at[n])
    prev_h <- if (n - 1 >= k) hours_between(h$run_at[k], h$run_at[n - 1]) else 0
    if (now_h >= FAIL_ALERT_HOURS && prev_h < FAIL_ALERT_HOURS)
      notify_text(sprintf("'%s' has failed for %.0f hours (%d runs): %s", s, now_h, n - k + 1, h$error[n]),
                  title = "Monitor: a site needs fixing")
  } else if (!ok[n - 1]) {
    k <- streak_start(n - 1)
    if (hours_between(h$run_at[k], h$run_at[n - 1]) >= FAIL_ALERT_HOURS)
      notify_text(sprintf("'%s' is working again (%d listings).", s, as.integer(h$n[n])), title = "Monitor: site recovered")
  }
}

# ---- write the log --------------------------------------------------------------------
cur_out <- scored |>
  transmute(id, source, url, title, address, postcode, city, lat, lon, rooms, m2, rent, aconto, rent_total,
            aconto_assumed, status, available_from, match, first_seen, alerted,
            detail_done, gone_at = NA_character_) |>
  mutate(across(everything(), as.character))
kept <- old |> filter(!id %in% cur_out$id) |> mutate(across(everything(), as.character))
if (nrow(kept)) {
  # gone = its site was scraped fine this run but the listing wasn't there
  gone_now <- kept$source %in% status$site[status$ok & !status$partial] & is.na(kept$gone_at)
  kept$gone_at[gone_now] <- now
}
merged <- bind_rows(cur_out, kept) |> select(-any_of("last_seen")) |> arrange(desc(first_seen), source, id)

if (!dry_run) {
  dir.create("data", showWarnings = FALSE)
  write_csv(merged, log_path, na = "")
  write_csv(stat_new, stat_path, na = "")
}
message(sprintf("%d listings on %d/%d sites, %d new, %d details fetched, %d alerts sent, %d matches queued",
                nrow(cur_out), sum(status$ok), nrow(status), sum(!cur_out$id %in% old$id),
                nrow(cand), if (no_alerts) 0 else nrow(pushed), if (no_alerts) nrow(to_alert) else max(0, nrow(to_alert) - nrow(pushed))))
