# cph-rental-monitor

Watches Copenhagen landlords' own websites for vacant rental flats and pushes new matches to your phone via [ntfy](https://ntfy.sh). Runs on GitHub Actions every 20 minutes; no aggregators (Boligportal etc.).

**Phone:** in the ntfy app, subscribe to the topic in `config/ntfy_topic.txt` (server ntfy.sh).

**What counts as a match:** `config/filters.yml` — size, rooms, max rent incl. aconto, postcodes (2300 is limited to an Amagerbro box), and keywords that never alert (rooms in shared flats, student/youth/senior housing). Edit it on GitHub; the next run uses it.

**Which sites:** `config/sites.csv` — one row per site; set `enabled` to `FALSE` to switch one off. Parsers live in `R/sites/` (`api_sites.R` for JSON APIs, `html_sites.R` for HTML pages, incl. the generic `links` and `textblocks` parsers that most small sites can reuse with just a URL and a link pattern).

**Overview:** `data/listings.csv` is every listing seen (open it on GitHub or in Excel): `match` = passes your filters, `alerted` = when it was pushed, `gone_at` = when it disappeared. `data/site_status.csv` shows each site's last runs; if a site fails 6 runs in a row (~2 h) you get a "site needs fixing" push.

**How a run works:** scrape all sites → carry over what earlier runs learned → read detail pages for possible matches (aconto, m², postcode; max 60 per run) → geocode 2300 addresses not given coordinates (OpenStreetMap Nominatim, cached in `data/geocode.csv`) → push matches not alerted before (max 25 per run) → commit the CSVs.

## Testing changes
```
Rscript R/monitor.R --dry-run                 # everything, no alerts, nothing written
Rscript R/monitor.R --dry-run --only=balder   # one site
Rscript R/monitor.R --no-alerts               # writes data/, sends nothing
```
Or trigger the workflow by hand: Actions → monitor → Run workflow. A new site is working when its row in `data/site_status.csv` shows `ok = TRUE` with a plausible `n`, and its rows in `data/listings.csv` have postcode, m², rooms and rent filled in.

Packages: httr2, rvest, xml2, jsonlite, dplyr, stringr, purrr, readr, tibble, yaml.
