# Geocoding with OpenStreetMap Nominatim, used only to tell Amagerbro apart from the rest of
# postcode 2300. Usage policy: max 1 request/second and an identifying user agent; results
# are cached in data/geocode.csv so each address is looked up once.
GEO_UA <- "cph-rental-monitor/1.0 (personal apartment alert; github.com/chr-heide/cph-rental-monitor)"
geo_cache_path <- "data/geocode.csv"

geo_key <- function(address, postcode) {
  street <- str_squish(str_remove(address, ",.*$"))            # "Hannemanns Allé 77, 1. 3" -> "Hannemanns Allé 77"
  str_to_lower(paste(street, coalesce(postcode, "")))
}

geocode_missing <- function(df, max_lookups = 25) {
  cache <- if (file.exists(geo_cache_path)) read_csv(geo_cache_path, col_types = "cdd") else
    tibble(key = character(), lat = double(), lon = double())
  need <- df |> filter(is.na(lat), !is.na(address)) |> mutate(key = geo_key(address, postcode)) |>
    distinct(key, address, postcode) |> filter(!key %in% cache$key) |> head(max_lookups)
  for (i in seq_len(nrow(need))) {
    street <- str_squish(str_remove(need$address[i], ",.*$"))
    q <- paste0(street, ", ", coalesce(need$postcode[i], ""), " København, Denmark")
    r <- tryCatch(
      request("https://nominatim.openstreetmap.org/search") |>
        req_url_query(q = q, format = "json", limit = 1, countrycodes = "dk") |>
        req_user_agent(GEO_UA) |> req_timeout(20) |> req_perform() |> resp_body_json(),
      error = function(e) NULL)
    hit <- if (length(r)) r[[1]] else NULL
    cache <- bind_rows(cache, tibble(key = need$key[i],
                                     lat = if (is.null(hit)) NA_real_ else as.numeric(hit$lat),
                                     lon = if (is.null(hit)) NA_real_ else as.numeric(hit$lon)))
    Sys.sleep(1.1)
  }
  if (nrow(need)) { dir.create("data", showWarnings = FALSE); write_csv(cache, geo_cache_path, na = "") }
  df |> mutate(key = geo_key(address, postcode)) |>
    left_join(rename(cache, lat_g = lat, lon_g = lon), by = "key") |>
    mutate(lat = coalesce(lat, lat_g), lon = coalesce(lon, lon_g)) |>
    select(-key, -lat_g, -lon_g)
}
