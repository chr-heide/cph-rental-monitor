# Parsers for sites whose listings are in the server-rendered HTML.
# `page` lets tests pass a saved page instead of fetching.

txt <- function(node, css) node |> html_element(css) |> html_text2() |> str_squish()
att <- function(node, css, a) node |> html_element(css) |> html_attr(a)

# ---- Kereby (Jorato WordPress plugin; data-* attributes on each card) ------------------
scrape_kereby <- function(url, param = NA, page = get_html(url)) {
  cards <- html_elements(page, "article.jorato-case-card")
  tibble(id = html_attr(cards, "data-card-id"),
         url = map_chr(cards, ~ att(.x, "a[href]", "href") %||% url),
         title = map_chr(cards, txt, ".jorato-case-card__headline"),
         address = map_chr(cards, txt, ".jorato-case-card__location-text"),
         postcode = html_attr(cards, "data-zip"), rooms = html_attr(cards, "data-rooms"),
         m2 = html_attr(cards, "data-size"), rent = html_attr(cards, "data-rent"),
         lat = html_attr(cards, "data-lat"), lon = html_attr(cards, "data-lng"),
         status = coalesce(html_attr(cards, "data-state"), "available"))
}
detail_kereby <- function(url) detail_generic(url)

# ---- Taurus (WordPress; data-* attributes) ------------------------------------------
scrape_taurus <- function(url, param = NA, page = get_html(url)) {
  a <- html_elements(page, "a.rental-item")
  loc <- map_chr(a, ~ txt(.x, ".details p"))
  tibble(id = str_extract(html_attr(a, "href"), "\\d+$"),
         url = url_absolute(html_attr(a, "href"), url),
         title = map_chr(a, txt, ".rental-title"),
         address = str_remove(loc, "^.*-\\s*"),
         city = str_replace_all(html_attr(a, "data-cities"), "-", " "),
         rooms = dk_num(html_attr(a, "data-rooms")), m2 = html_attr(a, "data-living-area"),
         rent = html_attr(a, "data-price"), status = "Ledig")
}
detail_taurus <- function(url) detail_generic(url)

# ---- Heimstaden (one page with all vacant flats in DK) ----------------------------------
scrape_heimstaden <- function(url, param = NA, page = get_html(url)) {
  a <- html_elements(page, "a.rental-element")
  det <- map(a, ~ html_elements(.x, ".details > div") |> html_text2() |> str_squish())
  pick <- function(d, re) { v <- str_subset(d, re); if (length(v)) v[1] else NA_character_ }
  h3 <- map_chr(a, ~ .x |> html_element("h3") |> html_text2() |> str_squish())
  tibble(id = map_chr(a, txt, ".rental-no"),
         url = url_absolute(html_attr(a, "href"), url),
         title = map_chr(a, txt, ".rental-type"),
         address = h3, postcode = map_chr(a, txt, ".rental-title-zip"),
         rent = dk_num(map_chr(det, pick, "kr")), rooms = dk_num(map_chr(det, pick, "vær")),
         m2 = dk_num(map_chr(det, pick, "m²|m2")),
         available_from = map_chr(det, pick, "[Ll]edig|\\d{2}[.-]\\d{2}"),
         status = "Ledig")
}
detail_heimstaden <- function(url) detail_generic(url)

# ---- Go'Bolig (SSR cards) ----------------------------------------------------------
scrape_gobolig <- function(url, param = NA, page = get_html(url)) {
  a <- html_elements(page, "a.apartment-list-card")
  attrs <- map(a, ~ html_elements(.x, ".attrs .attr-icon") |> html_text2() |> str_squish())
  pick <- function(d, re) { v <- str_subset(d, re); if (length(v)) v[1] else NA_character_ }
  tibble(id = html_attr(a, "href"), url = url_absolute(html_attr(a, "href"), url),
         title = map_chr(a, txt, ".title"), address = map_chr(a, txt, ".address"),
         m2 = dk_num(map_chr(attrs, pick, "m2|m²")), rooms = dk_num(map_chr(attrs, pick, "vær")),
         available_from = map_chr(attrs, pick, "\\d{2}\\.\\d{2}\\.\\d{4}|[Ss]narest"),
         rent = dk_num(map_chr(a, txt, ".pricing-wrap .val")), status = "Ledig")
}
detail_gobolig <- function(url) detail_generic(url)

# ---- HEA (WordPress archive; class "rented" = udlejet) ---------------------------------
scrape_hea <- function(url, param = NA, page = get_html(url)) {
  a <- html_elements(page, "a.archive-item")
  ttl <- html_attr(a, "title")
  tibble(id = html_attr(a, "href"), url = html_attr(a, "href"), title = ttl,
         address = map_chr(a, txt, "address"),
         rent = dk_num(map_chr(a, ~ { d <- html_elements(.x, ".data-item") |> html_text2(); v <- str_subset(d, "kr"); if (length(v)) v[1] else NA })),
         m2 = dk_num(str_match(ttl, "(\\d+)\\s*(m²|m2|kvm)")[, 2]),
         rooms = dk_num(str_match(ttl, "(\\d+[,½]?\\d?)\\s*-?\\s*vær")[, 2]),
         status = if_else(str_detect(html_attr(a, "class"), "rented"), "Udlejet", "Ledig"))
}
detail_hea <- function(url) detail_generic(url)

# ---- KAB "ledige boliger" (almene, direct letting; free-text cards) ----------------------
scrape_kab <- function(url, param = NA, page = get_html(url)) {
  cards <- html_elements(page, ".card")
  body <- map_chr(cards, ~ html_text2(html_element(.x, ".card-body")))
  keep <- !is.na(body) & str_detect(body, "(?i)husleje|m2|m²")
  cards <- cards[keep]; body <- body[keep]
  line <- function(re) str_squish(str_match(body, paste0("(?im)", re, "\\s*:?\\s*([^\\n]+)"))[, 2])
  tibble(id = paste(map_chr(cards, txt, ".card-header"), line("📍")),
         url = map_chr(cards, ~ { h <- att(.x, "a[href]", "href"); if (is.na(h) || str_starts(h, "mailto")) url else url_absolute(h, url) }),
         title = map_chr(cards, txt, ".card-header"),
         address = line("📍"),
         rooms = dk_num(str_match(body, "(\\d+)\\s*(rum|vær)")[, 2]),
         m2 = dk_num(str_match(body, "(\\d+[,.]?\\d*)\\s*m[2²]")[, 2]),
         rent = dk_num(line("Husleje")),
         aconto = dk_num(str_match(body, "(?i)aconto[^:\\n]{0,25}:\\s*([\\d.,]+)\\s*kr")[, 2]),
         available_from = line("Indflytning"),
         status = if_else(str_detect(body, "(?i)udlejet"), "Udlejet", "Ledig"))
}

# ---- Ejendomskontoret (Wix; parse text blocks) --------------------------------------
scrape_ejendomskontoret <- function(url, param = NA, page = get_html(url)) {
  t <- page |> html_element("body") |> html_text2()
  blocks <- str_split(t, "(?=Areal:)")[[1]][-1]
  prev <- str_split(t, "(?=Areal:)")[[1]]
  map_dfr(seq_along(blocks), function(i) {
    before <- str_split(str_trim(prev[i]), "\n")[[1]]; before <- before[nzchar(str_trim(before))]
    addr <- str_squish(paste(tail(before, 2), collapse = ", "))
    b <- blocks[i]
    addr <- str_remove(addr, "(?i)^til leje,?\\s*")
    tibble(id = addr, url = url, address = addr,
           m2 = dk_num(str_match(b, "Areal:\\s*([\\d,.]+)")[, 2]),
           rooms = dk_num(str_match(b, "Værelser:\\s*([\\d,½]+)")[, 2]),
           rent = dk_num(str_match(b, "([\\d.]+)\\s*kr")[, 2]), status = "Ledig")
  })
}

# ---- Kanalhuset (single building in Ørestad; table per apartment) -------------------------
scrape_kanalhuset <- function(url, param = NA, page = get_html(url)) {
  d <- html_elements(page, ".apartment-description")
  val <- function(node, label) {
    tr <- html_elements(node, "tr"); lab <- map_chr(tr, ~ html_text2(html_element(.x, "td")))
    v <- tr[str_detect(lab, label)]; if (length(v)) html_text2(html_element(v[1], ".field-value")) else NA_character_
  }
  tibble(id = map_chr(d, txt, ".apartment-name"), url = url, address = map_chr(d, txt, ".apartment-name"),
         postcode = "2300", city = "København S",
         rooms = dk_num(map_chr(d, val, "værelser")), m2 = dk_num(map_chr(d, val, "^Areal")),
         rent = dk_num(map_chr(d, val, "[Hh]usleje|[Ll]eje")),
         status = map_chr(d, ~ if (length(html_elements(.x, ".sold-title"))) html_text2(html_element(.x, ".sold-title")) else "Ledig"))
}

# ---- Express Housing (list of links; details on each page) -------------------------------
scrape_express_housing <- function(url, param = NA, page = get_html(url)) {
  links <- page |> html_elements("a[href*='/lejligheder/']") |> html_attr("href") |> url_absolute(url) |> unique()
  tibble(id = links, url = links,
         address = str_to_title(str_replace_all(str_remove(basename(links), "\\.aspx$"), "-", " ")),
         status = "Ledig")
}
detail_express_housing <- function(url) detail_generic(url)

# ---- Generic detail page reader ------------------------------------------------------
# Splits the page into text lines and reads values by label, taking the number on the
# same line after the label or else on the next line. Works across most landlord sites.
page_lines <- function(page) {
  # drop scripts and page chrome: search forms, menus, header and footer (agency addresses live there)
  for (n in html_elements(page, "script, style, noscript, svg, select, nav, form[role=search], form[class*=search], form[id*=search], form[action*=search], div[class*=prop_search]")) xml2::xml_remove(n)
  chrome <- "//footer[not(ancestor::main) and not(ancestor::article)] | //header[not(ancestor::main) and not(ancestor::article) and not(ancestor::section)]"
  for (n in xml2::xml_find_all(page, chrome)) xml2::xml_remove(n)
  t <- html_text2(html_element(page, "body"))
  l <- str_squish(str_split(str_replace_all(t, "[ ​]", " "), "\n")[[1]])
  l <- str_squish(str_remove_all(l, "\\((m2|m²|mdl\\.?|pr\\.? md\\.?)\\)"))
  l[nzchar(l)]
}
num_by_label <- function(lines, label, exclude = "$^", first_only = TRUE) {
  hit <- which(str_detect(lines, regex(label, ignore_case = TRUE)) & !str_detect(lines, regex(exclude, ignore_case = TRUE)))
  vals <- map_dbl(hit, function(i) {
    after <- str_remove(lines[i], regex(paste0("^.*?(", label, ")"), ignore_case = TRUE))
    v <- dk_num(str_extract(after, "\\d[\\d.]*(,\\d+)?"))
    if (is.na(v) && i < length(lines)) v <- dk_num(str_extract(lines[i + 1], "^[^A-Za-zÆØÅæøå]*\\d[\\d.]*(,\\d+)?"))
    v
  })
  vals <- vals[!is.na(vals)]
  if (!length(vals)) return(NA_real_)
  if (first_only) vals[1] else vals
}
detail_generic <- function(url, page = get_html(url)) {
  title <- paste(html_text2(html_element(page, "title")), html_text2(html_element(page, "h1")))
  L <- page_lines(page)
  not_monthly <- "første|forudbetal|depositum|indflytning|pr\\.? år|årlig|i alt"
  # monthly aconto items (heat, water, electricity, "forbrug", energy contribution)
  ac_labels <- c("A/C\\s*(varme|vand|el|forbrug)[^0-9]*", "aconto\\s*(varme|vand|el|forbrug|energibidrag)[^0-9]*",
                 "månedlig\\s*aconto[^0-9]*", "^aconto[^0-9]*$", "^a/c[^0-9]*$")
  ac <- map_dbl(ac_labels, ~ num_by_label(L, .x, exclude = not_monthly))
  ac <- unique(ac[!is.na(ac)])
  aconto <- if (length(ac)) sum(ac) else NA_real_
  first_month <- num_by_label(L, "aconto\\s*\\(?første måned\\)?[^0-9]*")   # often the full monthly aconto
  if (!is.na(first_month) && (is.na(aconto) || first_month > aconto)) aconto <- first_month
  yearly_heat <- num_by_label(L, "årlige\\s*(varme|forbrugs)omkostninger[^0-9]*")
  if (!is.na(yearly_heat)) aconto <- sum(aconto, yearly_heat / 12, na.rm = TRUE)
  m2 <- num_by_label(L, "^(bolig)?areal\\b[^0-9]*|^størrelse\\b[^0-9]*")
  if (is.na(m2)) m2 <- dk_num(str_match(paste(title, paste(head(L, 60), collapse = " ")), "(\\d{2,3})\\s*(m²|m2|m 2|kvm)")[, 2])
  rooms <- num_by_label(L, "^(antal\\s*)?værelser(\\s*/\\s*soveværelser)?\\b[^0-9]*")
  if (is.na(rooms)) rooms <- dk_num(str_match(paste(title, paste(head(L, 60), collapse = " ")), "(\\d)[ -]*værelses")[, 2])
  rent <- num_by_label(L, "^(månedlig\\s*)?husleje\\b[^0-9]*|^leje\\s*pr\\.?\\s*(måned|md)[^0-9]*|^(månedlig\\s*)?leje\\b[^0-9]*",
                       exclude = "første|forudbetal|depositum|indflytning|pr\\.? år|årlig|inkl")
  if (is.na(rent)) {   # unlabelled price like "22.000kr. /måned" or "22.900 DKK / md." (first one = the listing's own)
    p <- str_subset(L, "(?i)\\d[\\d.]*\\s*(kr\\.?|dkk|,-)\\s*/?\\s*(pr\\.?\\s*)?(md|måned|mdr|month)")
    if (length(p)) rent <- dk_num(p[1])
  }
  # address/postcode from the title or heading ("Street 1, 2. th, 2100 København Ø"), not the footer
  pc <- str_match(title, ",\\s*(\\d{4})\\s+[A-ZÆØÅ]")[, 2]
  h1 <- str_squish(html_text2(html_element(page, "h1")))
  addr <- if (!is.na(h1) && str_detect(h1, "^[A-ZÆØÅ][A-Za-zÆØÅæøå .'-]+ \\d+[A-Z]?(,|$)")) h1 else NA_character_
  if (is.na(pc)) {   # fall back to the first "street no, NNNN City" line on the page
    a <- str_subset(L, "^[A-ZÆØÅ][^,]{2,40}\\d+[^,]*,.*\\b\\d{4}\\s+[A-ZÆØÅ]")
    if (length(a)) { pc <- str_match(a[1], "\\b(\\d{4})\\s+[A-ZÆØÅ]")[, 2]; addr <- coalesce(addr, a[1]) }
  }
  if (is.na(pc)) {   # last resort: a district name near the top of the page ("København Ø")
    top <- head(L, max(15, round(length(L) * 0.4)))
    d <- str_subset(top, "K(ø|o)benhavn (K|V|N|NV|Ø|S|SV)\\b|Nordhavn|Frederiksberg|Valby")
    if (length(d)) pc <- city_postcode(str_extract(d[1], "K(ø|o)benhavn (K|V|N|NV|Ø|S|SV)\\b|Nordhavn|Frederiksberg|Valby"))
  }
  list(aconto = aconto, m2 = m2, rooms = rooms, rent = rent, postcode = pc, address = addr)
}

# ---- Generic: collect detail links from a list page ----------------------------------
# param = "<regex for detail links>" or "<regex>|<max pages>" (follows WordPress-style /page/N/)
scrape_links <- function(url, param = NA, page = get_html(url)) {
  pr <- str_split(param, "\\|")[[1]]; pat <- pr[1]; pages <- if (length(pr) > 1) as.integer(pr[2]) else 1
  grab <- function(pg, base) {
    a <- html_elements(pg, "a[href]")
    tibble(href = url_absolute(html_attr(a, "href"), base),
           text = str_squish(str_remove_all(html_text2(a), "\\.[A-Za-z0-9_-]+\\{[^}]*\\}"))) |>   # drop inline-SVG CSS
      filter(str_detect(href, pat))
  }
  out <- grab(page, url)
  if (pages > 1) for (k in 2:pages) {
    pk <- tryCatch(get_html(paste0(str_remove(url, "/$"), "/page/", k, "/")), error = function(e) NULL)
    if (is.null(pk)) break
    more <- grab(pk, url); if (!nrow(more)) break
    out <- bind_rows(out, more); Sys.sleep(0.5)
  }
  generic <- "(?i)^(se|læs|vis|klik)\\b.{0,25}$|^(læs mere|se mere|mere info)$"
  out <- out |> mutate(text = if_else(str_detect(str_squish(str_remove_all(text, "[\u200b]")), generic), "", text)) |>
    group_by(href) |> summarise(text = paste(unique(text[nzchar(text)]), collapse = " | "), .groups = "drop")
  slug <- str_replace_all(str_remove(basename(str_remove(out$href, "/$")), "\\.(aspx?|html?|php)$"), "[-_]", " ")
  t <- paste(out$text, slug)
  tibble(id = out$href, url = out$href, title = str_trunc(coalesce(na_if(out$text, ""), str_to_sentence(slug)), 120),
         m2 = dk_num(str_match(t, "(\\d{2,3})\\s*(m²|m2|kvm)")[, 2]),
         rooms = dk_num(str_match(t, "(\\d)[ -]*(vær|værelses|rum)")[, 2]),
         postcode = coalesce(postcode_of(t), city_postcode(str_extract(t, "(?i)k(ø|o)benhavn (k|v|n|nv|ø|oe|s|sv)\\b|nordhavn|frederiksberg|valby"))),
         rent = dk_num(str_match(t, "([\\d.]{4,})\\s*(kr|,-)")[, 2]),
         status = if_else(str_detect(str_to_lower(t), "udlejet|reserveret"), "Udlejet", "Ledig"))
}
detail_links <- function(url) detail_generic(url)

# ---- Generic: listings as text blocks ("Street 1, 2. th, 2450 København SV" then m2/rooms/rent) ----
scrape_textblocks <- function(url, param = NA, page = get_html(url)) {
  L <- page_lines(page)
  i <- which(str_detect(L, "^[A-ZÆØÅ][^,]{2,40}\\d+[^,]*,.*\\b\\d{4}\\s+[A-ZÆØÅ]") & nchar(L) < 90)
  map_dfr(i, function(k) {
    nxt <- paste(L[(k + 1):min(length(L), k + 6)], collapse = " ¦ ")
    nxt <- str_split(nxt, "\\b\\d{4}\\s+[A-ZÆØÅ]")[[1]][1]   # stop at the next address
    tibble(id = L[k], url = url, address = L[k],
           m2 = dk_num(str_match(nxt, "(\\d+[,.]?\\d*)\\s*(m²|m2)")[, 2]),
           rooms = dk_num(str_match(nxt, "(\\d)\\s*(værelser|vær\\.|rum)")[, 2]),
           rent = dk_num(str_match(nxt, "([\\d.]{4,})\\s*(kr|,-)")[, 2]),
           status = if_else(str_detect(str_to_lower(nxt), "udlejet|reserveret"), "Udlejet", "Ledig"))
  })
}

# ---- EDC (estate agent; SSR cards, newest first). param = pages to read per run ----------
scrape_edc <- function(url, param = NA) {
  pages <- as.integer(coalesce(param, "4"))
  out <- list()
  for (k in seq_len(pages)) {
    pg <- get_html(paste0(url, if (str_detect(url, "\\?")) "&" else "?", "side=", k))
    cards <- html_elements(pg, "article")
    href <- map_chr(cards, ~ att(.x, "a[href*='/leje/']", "href"))
    keep <- !is.na(href)
    if (!any(keep)) break
    cards <- cards[keep]; href <- href[keep]
    facts <- map_chr(cards, ~ paste(html_text2(html_elements(.x, "address ~ span, address + span")), collapse = " "))
    out[[k]] <- tibble(id = str_extract(href, "\\d+/?$") |> str_remove("/"), url = url_absolute(href, url),
                       address = map_chr(cards, txt, "address h2"),
                       postcode = str_extract(map_chr(cards, txt, "address span"), "\\d{4}"),
                       city = str_remove(map_chr(cards, txt, "address span"), "^\\d{4}\\s*"),
                       rooms = dk_num(str_match(facts, "(\\d+)\\s*rum")[, 2]),
                       m2 = dk_num(str_match(facts, "(\\d+)\\s*m²")[, 2]),
                       rent = dk_num(str_match(facts, "([\\d.]+)\\s*kr")[, 2]),
                       title = map_chr(cards, txt, "a[href*='/leje/'].text-xs"), status = "Ledig")
    Sys.sleep(2)
  }
  structure(bind_rows(out), partial = TRUE)
}
detail_edc <- function(url) detail_generic(url)

# ---- home (estate agent; SSR cards, all pages for Copenhagen) ---------------------------
scrape_home <- function(url, param = NA) {
  pages <- as.integer(coalesce(param, "30")); out <- list()
  for (k in seq_len(pages)) {
    pg <- get_html(paste0(url, "?page=", k))
    det <- html_elements(pg, ".property-card-details")
    if (!length(det)) break
    card <- map(det, ~ xml2::xml_parent(xml2::xml_parent(.x)))
    href <- map_chr(card, ~ { a <- html_elements(.x, "a[href*='/til-leje/']") |> html_attr("href"); if (length(a)) a[1] else NA_character_ })
    if (all(is.na(href))) href <- map_chr(det, ~ { a <- html_elements(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(.x))), "a[href]") |> html_attr("href"); a <- a[!str_detect(a, "^#|javascript")]; if (length(a)) a[1] else NA_character_ })
    bits <- map(det, ~ html_elements(.x, ".property-card-details__description p") |> html_text2() |> str_squish())
    pick <- function(b, re) { v <- str_subset(b, re); if (length(v)) v[1] else NA_character_ }
    addr <- map_chr(det, txt, ".property-card-details__address")
    out[[k]] <- tibble(id = coalesce(href, addr), url = url_absolute(coalesce(href, url), url), address = addr,
                       title = map_chr(bits, ~ .x[1] %||% NA_character_),
                       m2 = dk_num(map_chr(bits, pick, "m2|m²")), rent = dk_num(map_chr(bits, pick, "kr")),
                       status = "Ledig")
    Sys.sleep(0.7)
  }
  bind_rows(out)
}
detail_home <- function(url) detail_generic(url)

# ---- VIBO (almen; "ledige boliger" page is free text per flat) ---------------------------
scrape_vibo <- function(url, param = NA, page = get_html(url)) {
  L <- page_lines(page)
  i <- which(str_detect(L, "(?i)bolig på .+\\b\\d{4}\\s+[A-ZÆØÅ]") & nchar(L) < 160)
  i <- i[!duplicated(L[i])]
  map_dfr(i, function(k) {
    blk <- paste(L[k:min(length(L), k + 8)], collapse = " ¦ ")
    nxt <- str_split(paste(L[(k + 1):min(length(L), k + 8)], collapse = " ¦ "), "(?i)bolig på ")[[1]][1]
    tibble(id = L[k], url = url, title = L[k],
           address = str_squish(str_match(L[k], "(?i)bolig på (.+?\\b\\d{4}\\s+[A-ZÆØÅ][a-zæøå]+(?:\\s[A-ZÆØÅ]{1,2}\\b)?)")[, 2]),
           rooms = dk_num(str_match(L[k], "(\\d)[ -]*(værelses|rums)")[, 2]),
           m2 = dk_num(str_match(nxt, "(\\d+[,.]?\\d*)\\s*(m²|m2|kvm)")[, 2]),
           rent = dk_num(str_match(nxt, "(?i)husleje[^0-9]{0,15}([\\d.,]+)")[, 2]),
           aconto = { a <- dk_num(str_match_all(nxt, "(?i)aconto[^0-9]{0,20}([\\d.,]+)")[[1]][, 2]); if (length(a)) sum(a) else NA_real_ },
           available_from = str_match(L[k], "(?i)ledig pr\\.?\\s*(.+)$")[, 2],
           status = "Ledig")
  }) |> arrange(is.na(rent)) |> distinct(address, .keep_all = TRUE) |> mutate(id = address)
}
