# Scrape every enabled site, log all listings, push alerts for matches not alerted before.
# Run from repo root:  Rscript R/monitor.R               (normal run)
#                      Rscript R/monitor.R --dry-run     (no alerts, no state writes)
#                      Rscript R/monitor.R --no-alerts   (writes the log, sends nothing)
#                      Rscript R/monitor.R --only=kereby,taurus
source("R/utils.R")
for (f in list.files("R/sites", full.names = TRUE, pattern = "\\.R$")) source(f)
source("R/notify.R")
library(yaml)
if (nzchar(Sys.getenv("MONITOR_MOCK"))) source(Sys.getenv("MONITOR_MOCK"))  # tests: serve saved pages

args    <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
no_alerts <- dry_run || "--no-alerts" %in% args
only_arg <- grep("^--only=", args, value = TRUE)
only    <- if (length(only_arg)) str_split(str_remove(only_arg[1], "^--only="), ",")[[1]] else character(0)
now     <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "Europe/Copenhagen")
MAX_DETAIL <- 40   # detail pages fetched per run (all sites together)
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
  res <- tryCatch(as_listings(get(paste0("scrape_", s$parser))(s$url, s$param), s$name),
                  error = function(e) e)
  ok <- !inherits(res, "error") && nrow(res) > 0
  if (ok) results[[s$name]] <- res
  err <- if (inherits(res, "error")) conditionMessage(res) else if (!ok) "0 listings parsed" else NA_character_
  status[[s$name]] <- tibble(site = s$name, run_at = now, ok = ok, n = if (ok) nrow(res) else 0L,
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
prev <- old |> select(id, all_of(num_cols), postcode, available_from, first_seen, alerted, detail_done) |>
  mutate(across(all_of(num_cols), as.numeric)) |> rename_with(~ paste0(.x, "_old"), -id)
cur <- cur |> left_join(prev, by = "id") |>
  mutate(rooms = coalesce(rooms, rooms_old), m2 = coalesce(m2, m2_old), rent = coalesce(rent, rent_old),
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
score <- function(df) {
  unk <- isTRUE(filt$alert_on_unknown)
  df |> mutate(
    aconto_assumed = is.na(aconto) & !is.na(rent),
    rent_total     = rent + coalesce(aconto, 0),
    rent_check     = if_else(aconto_assumed, rent + filt$assumed_aconto_if_missing, rent_total),
    ok_area  = in_ranges(postcode, filt$postcodes),
    ok_m2    = if_else(is.na(m2), unk, m2 >= filt$min_m2),
    ok_rooms = if_else(is.na(rooms), unk, rooms >= filt$min_rooms),
    ok_rent  = if_else(is.na(rent_check), unk, rent_check <= filt$max_rent_total),
    ok_avail = !str_detect(coalesce(str_to_lower(status), ""), unavailable_re),
    ok_kw    = !str_detect(str_to_lower(paste(title, address)), excl_re),
    match    = ok_area & ok_m2 & ok_rooms & ok_rent & ok_avail & ok_kw)
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

scored <- score(cur)

# ---- alerts -------------------------------------------------------------------------
to_alert <- scored |> filter(match, is.na(alerted)) |> arrange(rent_check)
pushed <- head(to_alert, MAX_PUSH)
if (!no_alerts && nrow(pushed)) {
  for (i in seq_len(nrow(pushed))) notify_listing(pushed[i, ])
  if (nrow(to_alert) > MAX_PUSH)
    notify_text(sprintf("%d more matches queued for the next run(s).", nrow(to_alert) - MAX_PUSH), title = "More matches")
  scored$alerted[scored$id %in% pushed$id] <- now
}

# ---- site health: one alert when a site has failed 6 runs in a row (~2 h) ----------
stat_path <- "data/site_status.csv"
stat_old <- if (file.exists(stat_path)) read_csv(stat_path, col_types = cols(.default = "c")) else tibble()
stat_new <- bind_rows(stat_old, mutate(status, across(everything(), as.character))) |>
  group_by(site) |> slice_tail(n = 30) |> ungroup()
if (!no_alerts) for (s in status$site[!status$ok]) {
  h <- stat_new |> filter(site == s)
  streak <- rle(rev(h$ok))
  if (streak$values[1] == "FALSE" && streak$lengths[1] == 6)
    notify_text(paste0("'", s, "' has failed 6 runs in a row: ", tail(h$error, 1)), title = "Monitor: a site needs fixing")
}

# ---- write the log --------------------------------------------------------------------
cur_out <- scored |>
  transmute(id, source, url, title, address, postcode, city, rooms, m2, rent, aconto, rent_total,
            aconto_assumed, status, available_from, match, first_seen, last_seen = now, alerted,
            detail_done, gone_at = NA_character_) |>
  mutate(across(everything(), as.character))
kept <- old |> filter(!id %in% cur_out$id) |> mutate(across(everything(), as.character))
if (nrow(kept)) {
  # gone = its site was scraped fine this run but the listing wasn't there
  gone_now <- kept$source %in% status$site[status$ok] & is.na(kept$gone_at)
  kept$gone_at[gone_now] <- now
}
merged <- bind_rows(cur_out, kept) |> arrange(desc(first_seen), source)

if (!dry_run) {
  dir.create("data", showWarnings = FALSE)
  write_csv(merged, log_path, na = "")
  write_csv(stat_new, stat_path, na = "")
}
message(sprintf("%d listings on %d/%d sites, %d new, %d details fetched, %d alerts sent, %d matches queued",
                nrow(cur_out), sum(status$ok), nrow(status), sum(!cur_out$id %in% old$id),
                nrow(cand), if (no_alerts) 0 else nrow(pushed), if (no_alerts) nrow(to_alert) else max(0, nrow(to_alert) - nrow(pushed))))
