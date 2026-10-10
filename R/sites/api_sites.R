# Parsers for sites that load listings from a JSON API.
# Each scrape_<parser>(url, param) returns a data frame with (a subset of) LISTING_COLS.

# Public keys that sites embed in their JS bundles can rotate. find_bundle_key() pulls the
# page's script bundles and returns the first match of `pattern`; cached per run.
.key_cache <- new.env()
find_bundle_key <- function(page_url, pattern, near = NULL) {
  ck <- paste(page_url, pattern)
  if (!is.null(.key_cache[[ck]])) return(.key_cache[[ck]])
  pg <- get_html(page_url)
  srcs <- pg |> html_elements("script[src]") |> html_attr("src")
  srcs <- unique(url_absolute(srcs[!str_detect(srcs, "google|cookie|hotjar|consent|gtag|facebook|strossle")], page_url))
  inline <- pg |> html_elements("script:not([src])") |> html_text() |> paste(collapse = "\n")
  for (txt in c(list(inline), as.list(srcs))) {
    body <- if (identical(txt, inline)) inline else tryCatch(resp_body_string(req_get(txt)), error = function(e) "")
    if (!is.null(near)) {
      pos <- str_locate_all(body, near)[[1]][, 1]
      cand <- unlist(lapply(pos, function(p) str_extract_all(str_sub(body, max(1, p - 400), p + 400), pattern)[[1]]))
    } else cand <- str_extract_all(body, pattern)[[1]]
    if (length(cand)) { .key_cache[[ck]] <- cand[1]; return(cand[1]) }
  }
  stop("could not find key in bundles of ", page_url)
}

# ---- Jorato (api.jorato.com) — used by e.g. Norse Property Management ----------
# param = landlord's public Jorato key (fallback: discovered from the site's JS bundle)
scrape_jorato <- function(url, param = NA) {
  fetch <- function(key) get_json(paste0("https://api.jorato.com/tenancies?offset=0&limit=500&key=", key), simplifyVector = FALSE)
  key <- if (!is.na(param) && nzchar(param)) param else find_bundle_key(url, "[A-Za-z0-9]{32}", near = "jorato")
  d <- tryCatch(fetch(key), error = function(e) {
    key <<- find_bundle_key(url, "[A-Za-z0-9]{32}", near = "jorato"); fetch(key) })
  site <- str_extract(url, "^https?://[^/]+")
  map_dfr(d$items, function(it) {
    if (!identical(it$classification, "Residential")) return(NULL)
    exp <- sum(map_dbl(it$expenses %||% list(), ~ .x$amount$value %||% 0))
    street <- it$address$street %||% NA
    tibble(id = it$id,
           url = paste0(site, "/bolig/", str_remove_all(street, "\\s"), "+", str_remove_all(it$id, "-")),
           title = it$title %||% NA, address = street,
           postcode = it$address$zipCode %||% NA, city = it$address$city %||% NA,
           rooms = it$rooms %||% NA, m2 = it$size$value %||% NA,
           rent = it$monthlyRent$value %||% NA, aconto = exp,
           status = it$state %||% NA, available_from = it$availableFrom %||% NA,
           lat = it$address$coordinates$latitude %||% NA, lon = it$address$coordinates$longitude %||% NA)
  })
}

# ---- Balder (Meilisearch at api.balder.dk) ---------------------------------------
scrape_balder <- function(url, param = NA) {
  q <- list(queries = list(list(indexUid = "leases", filter = list("status = 'Ledig'"), limit = 3000, offset = 0)))
  call <- function(key) request("https://api.balder.dk/multi-search") |>
    req_user_agent(UA) |> req_auth_bearer_token(key) |> req_body_json(q) |>
    req_timeout(40) |> req_perform() |> resp_body_json(simplifyVector = FALSE)
  key <- if (!is.na(param) && nzchar(param)) param else find_bundle_key(url, "\\b[0-9a-f]{64}\\b", near = "multi-search|meili|api\\.balder")
  d <- tryCatch(call(key), error = function(e) call(find_bundle_key(url, "\\b[0-9a-f]{64}\\b")))
  map_dfr(d$results[[1]]$hits, function(h) tibble(
    id = h$id, url = paste0("https://www.balder.dk/lejeboliger/", h$slug %||% ""),
    title = h$headline_da %||% h$headline %||% NA, address = h$street %||% NA,
    postcode = as.character(h$postal_code %||% NA), city = h$city %||% NA,
    rooms = h$number_of_rooms %||% NA, m2 = h$gross_area %||% NA, rent = h$rent %||% NA,
    aconto = (h$payment_water %||% 0) + (h$payment_heating %||% 0),
    status = h$status %||% NA, available_from = h$acquisition_date %||% NA,
    lat = h$`_geo`$lat %||% NA, lon = h$`_geo`$lng %||% NA))
}

# ---- Jeudan (nova-api.jeudan.dk) --------------------------------------------------
scrape_jeudan <- function(url, param = NA) {
  d <- get_json("https://nova-api.jeudan.dk/api/leases/search?lease_type=bolig&per_page=200", simplifyVector = FALSE)
  map_dfr(d$data, function(x) {
    txt <- paste(x$heading, x$summary, x$description)
    tibble(id = as.character(x$id), url = paste0("https://www.jeudan.dk", x$statamic_url %||% ""),
           title = x$heading %||% NA, address = x$title %||% NA,
           postcode = as.character(x$zip_code %||% NA), city = x$city_name %||% NA,
           rooms = dk_num(str_match(txt, "(\\d+)\\s*(?:v&aelig;relser|værelser|vær\\.)")[, 2]),
           m2 = x$lease_type_square_meters %||% NA,
           rent = x$total_monthly_rent_incl %||% x$lease_type_monthly_rent %||% NA,
           status = if (isTRUE(x$is_rented_out)) "Udlejet" else "Ledig",
           lat = x$latitude %||% NA, lon = x$longitude %||% NA)
  })
}

# ---- Danica Ejendomme (bolig.danicaejendomme.dk) ------------------------------------
scrape_danica <- function(url, param = NA) {
  body <- list(page = list(PageNo = 1, PageSize = 500, TotalRecords = 0, TotalPages = 0, Pages = list(),
                           SortOrder = "", IsAsc = TRUE, SearchText = "",
                           Criteria = list(SearchText = "", SearchPType = "Lejebolig", SearchPArea = "")),
               pStatus = "Til leje", pType = "", pArea = "")
  d <- request("https://bolig.danicaejendomme.dk/api/admin/Property/list/") |> req_user_agent(UA) |>
    req_body_json(body) |> req_timeout(40) |> req_perform() |> resp_body_json(simplifyVector = FALSE)
  map_dfr(d$Data$List, function(x) {
    if (isTRUE(x$IsWaitingList) || isTRUE(x$IsBusinessProperty)) return(NULL)
    acon <- sum(c(x$AccountHeat, x$AccountWater) %||% 0, na.rm = TRUE)
    tibble(id = x$CaseNo, url = x$PresentationUrl %||% paste0("https://bolig.danicaejendomme.dk/hjem/findbolig?search=", x$CaseNo),
           title = x$CaseName %||% NA,
           address = str_squish(paste(x$Road %||% "", x$HouseNo %||% "", if (nzchar(x$Floor %||% "")) paste0(", ", x$Floor) else "")),
           postcode = as.character(x$ZipCode %||% NA), city = x$City %||% NA,
           rooms = x$RoomCount %||% NA, m2 = x$Area %||% NA,
           rent = x$MonthlyRent %||% NA, aconto = if (acon > 0) acon else NA,
           status = if (isTRUE(x$Reserved)) "Reserveret" else x$Status %||% NA,
           available_from = x$AvailableDate %||% NA,
           lat = as.numeric(str_split(x$GpsCoordinates %||% "NA NA", " ")[[1]][1]),
           lon = as.numeric(str_split(x$GpsCoordinates %||% "NA NA", " ")[[1]][2]))
  })
}

# ---- Juli Living / Nrep (juliliving.dk admin-ajax) -----------------------------------
scrape_juli <- function(url, param = NA) {
  pg <- get_html(url)
  eid <- pg |> html_element(".js-jlet-units") |> html_attr("data-e-id")
  if (is.na(eid)) stop("no data-e-id on page")
  d <- get_json(paste0("https://juliliving.dk/wp-admin/admin-ajax.php?action=jlet_units&locale=da&e_id=",
                       URLencode(eid, reserved = TRUE)), simplifyVector = FALSE)
  map_dfr(d$data$units, function(u) tibble(
    id = as.character(u$id), url = u$url %||% NA, title = u$Headline %||% NA,
    address = str_squish(paste(u$address %||% "")), postcode = as.character(u$postcode %||% NA),
    city = u$ZipCity %||% NA, rooms = u$rooms %||% NA, m2 = u$area %||% NA,
    rent = u$price %||% NA, status = u$StatusText %||% NA, available_from = u$VacantDate %||% NA))
}

# ---- DAB / Lejerbo almene "ledige boliger" (dab-lejerbo.dk) -----------------------------
scrape_dab_lejerbo <- function(url, param = NA) {
  body <- list(selectedNumberOfRooms = 0, selectedMinimumSize = 0, selectedMaxRent = 0,
               selectedMainRegion = "", selectedRegion = "", fallbackDescriptionToDepartmentDescription = FALSE,
               pageNumber = 1, pageSize = 1000, shuffleSeed = "a", sortBy = "moveInDate")
  d <- request("https://dab-lejerbo.dk/api/memberportal/availabletenancies/getAllAvailableTenancies") |>
    req_user_agent(UA) |> req_body_json(body) |> req_timeout(60) |> req_perform() |>
    resp_body_json(simplifyVector = FALSE)
  map_dfr(d$data$data$availableTenancies, function(t) tibble(
    id = paste(t$companyId, t$departmentId, t$tenancyId, sep = "-"),
    url = if (nzchar(t$departmentUrl %||% "")) t$departmentUrl else url,
    title = t$description %||% NA,
    address = str_squish(paste(t$address$street %||% "", t$address$location %||% "")),
    postcode = as.character(t$address$zipCode %||% NA), city = t$address$city %||% NA,
    rooms = t$rooms %||% NA, m2 = t$sqm %||% NA, rent = t$rentNet %||% NA,
    status = "Ledig", available_from = t$moveInDate %||% NA))
}

# ---- findbolig.nu (DEAS platform: PFA, Velliv, ATP, PensionDanmark and others) -------------
# Only "Advert" residences are directly rentable; everything else on findbolig is a waitlist.
# findbolig.nu's server omits its intermediate certificate. Browsers fetch it themselves (AIA);
# curl doesn't, so we fetch it the same way and add it to a CA bundle. Verification stays on.
.fb_cainfo <- NULL
findbolig_cainfo <- function() {
  if (!is.null(.fb_cainfo)) return(.fb_cainfo)
  leaf <- system("echo | openssl s_client -connect findbolig.nu:443 -servername findbolig.nu 2>/dev/null | openssl x509 -noout -text",
                 intern = TRUE)
  aia <- str_match(paste(leaf, collapse = "\n"), "CA Issuers - URI:(\\S+)")[, 2]
  if (is.na(aia)) stop("findbolig: no AIA issuer URL in certificate")
  der <- tempfile(fileext = ".der"); pem <- tempfile(fileext = ".pem"); bundle <- tempfile(fileext = ".pem")
  request(aia) |> req_perform(path = der)
  if (system2("openssl", c("x509", "-inform", "DER", "-in", der, "-out", pem)) != 0)
    file.copy(der, pem, overwrite = TRUE)   # some AIA URLs already serve PEM
  sys_ca <- c("/etc/ssl/certs/ca-certificates.crt", "/etc/pki/tls/certs/ca-bundle.crt")
  writeLines(c(readLines(sys_ca[file.exists(sys_ca)][1]), readLines(pem)), bundle)
  .fb_cainfo <<- bundle
  bundle
}
scrape_findbolig <- function(url, param = NA) {
  call <- function(page, cainfo = NULL) {
    r <- request("https://findbolig.nu/api/search") |> req_user_agent(UA) |>
      req_body_json(list(pageSize = 100, page = page, orderBy = "Created", orderDirection = "DESC",
                         mixedResults = FALSE, filters = list(RentModel = list("Advert")))) |> req_timeout(40)
    if (!is.null(cainfo)) r <- req_options(r, cainfo = cainfo)
    resp_body_json(req_perform(r), simplifyVector = FALSE)
  }
  first <- tryCatch(call(0), error = function(e) {
    if (!str_detect(conditionMessage(e), "(?i)ssl|certificate|issuer")) stop(e)
    call(0, findbolig_cainfo())
  })
  ca <- .fb_cainfo
  res <- first$results
  pages <- ceiling((first$totalResults %||% 0) / 100)
  if (pages > 1) for (p in 1:(pages - 1)) res <- c(res, call(p, ca)$results)
  findbolig_rows(res)
}
findbolig_rows <- function(res) {
  map_dfr(res, function(x) {
    if (!identical(x[["$type"]], "Residence") || !identical(x$rentModel, "Advert")) return(NULL)
    door <- paste(c(x$floor, x$door)[!map_lgl(list(x$floor, x$door), is.null)], collapse = " ")
    tibble(id = as.character(x$shortId), url = paste0("https://findbolig.nu/da-dk/residence/", x$shortId),
           title = paste(x$propertyCompanyName %||% "", "-", x$propertyName %||% ""),
           address = str_squish(paste0(x$street %||% "", " ", x$number %||% "", x$letter %||% "",
                                       if (nzchar(door)) paste0(", ", door) else "")),
           postcode = as.character(x$postalCode %||% NA), city = x$city %||% NA,
           rooms = x$rooms %||% NA, m2 = x$area %||% NA, rent = x$rent %||% NA,
           aconto = if (is.null(x$aconto) || x$aconto == 0) NA_real_ else x$aconto,
           status = if (!is.null(x$activeOfferId)) "Tilbudt" else "Ledig",
           available_from = str_sub(x$availableFrom %||% NA, 1, 10),
           lat = x$latitude %||% NA, lon = x$longitude %||% NA)
  })
}

# ---- Nybolig (estate agent; rental cases via its search API) -----------------------------
# param = postal-code ranges to ask for, e.g. "1000-2450" (keeps the request to Copenhagen)
scrape_nybolig <- function(url, param = NA) {
  rng <- as.integer(str_split(coalesce(param, "1000-2450"), "-")[[1]])
  out <- list(); token <- ""
  for (k in 1:15) {
    body <- list(siteName = "nybolig", isRental = TRUE, isSold = FALSE, isSaleInProgress = FALSE,
                 postalCodeRanges = list(list(from = rng[1], to = rng[2])),
                 municipalityCodes = list(), municipalityNames = list(), cityNames = list(),
                 supplementaryCityNames = list(), postalCodes = list(), streetNames = list(),
                 streetNumbers = list(), propertyCategories = list(), businessPropertyCategories = list(),
                 polygon = list(), freeText = "", sort = 0, scrollToken = token, top = 100)
    d <- request("https://www.nybolig.dk/api/search/cases/find") |> req_user_agent(UA) |>
      req_body_json(body) |> req_timeout(40) |> req_perform() |> resp_body_json(simplifyVector = FALSE)
    out <- c(out, d$cases)
    token <- d$scrollToken %||% ""
    if (!length(d$cases) || !nzchar(token) || length(out) >= (d$total %||% 0)) break
    Sys.sleep(0.5)
  }
  map_dfr(out, function(c) {
    if (!isTRUE(c$isRental)) return(NULL)
    tibble(id = c$caseNumber %||% c$id, url = paste0("https://www.nybolig.dk", c$url %||% ""),
           title = c$imageAlt %||% NA, address = c$addressDisplayName %||% NA,
           postcode = str_extract(c$addressSecondLine %||% "", "^\\d{4}"), city = c$addressCity %||% NA,
           rooms = c$totalNumberOfRooms %||% NA, m2 = c$livingSpace %||% NA,
           rent = { r <- c$rent %||% 0; if (r > 0) r else dk_num(c$rentMonthly) },
           status = if (isTRUE(c$projectCaseReserved)) "Reserveret" else "Ledig",
           lat = c$addressLatitude %||% NA, lon = c$addressLongitude %||% NA)
  })
}
detail_nybolig <- function(url) detail_generic(url)

# ---- Norhjem (norhjem.dk/api/searchrental, form-encoded POST) ---------------------------
scrape_norhjem <- function(url, param = NA) {
  d <- request("https://norhjem.dk/api/searchrental") |> req_user_agent(UA) |>
    req_body_form(availableFrom = "", availableTo = "", propertyId = "", rentalType = "", maxPrice = "",
                  minRooms = "", maxRooms = "", area = "", sort = "") |>
    req_timeout(40) |> req_perform() |> resp_body_json(simplifyVector = FALSE)
  map_dfr(d, function(x) {
    if (isTRUE(x$isParking)) return(NULL)
    tibble(id = x$url, url = paste0("https://norhjem.dk", x$url), title = x$type %||% NA,
           address = x$address %||% NA, postcode = as.character(x$zipCode %||% NA), city = x$city %||% NA,
           rooms = x$rooms %||% NA, m2 = dk_num(x$area), rent = x$price %||% NA,
           status = x$status %||% NA, available_from = str_sub(x$moveInDate %||% NA, 1, 10))
  })
}
detail_norhjem <- function(url) detail_generic(url)
