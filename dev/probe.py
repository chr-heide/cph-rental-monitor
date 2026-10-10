"""Dev probe: render candidate landlord sites, save HTML, JSON responses, robots.txt
and listing-looking links, so parsers can be designed from real data."""
import json, re, sys, pathlib, urllib.parse, urllib.request
from playwright.sync_api import sync_playwright

OUT = pathlib.Path("probe_out")
KW = re.compile(r"ledig|lejebolig|leje-bolig|find-?bolig|boliger|udlejning|lejlighed|lejemaal|lejemål|til-leje|rent", re.I)
NOFOLLOW = True
UA = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0 Safari/537.36"

sites = [l.split("|", 1) for l in pathlib.Path(sys.argv[1]).read_text().splitlines()
         if l.strip() and not l.startswith("#")]
shard = [a for a in sys.argv[2:] if "/" in a]
only = [a for a in sys.argv[2:] if "/" not in a]  # optional subset of names
if shard:
    k, n = map(int, shard[0].split("/")); sites = sites[k::n]

def robots(url):
    p = urllib.parse.urlparse(url)
    try:
        req = urllib.request.Request(f"{p.scheme}://{p.netloc}/robots.txt", headers={"User-Agent": UA})
        return urllib.request.urlopen(req, timeout=15).read(20000).decode("utf-8", "replace")
    except Exception as e:
        return f"ERR {e}"

def visit(page, url, d, tag):
    jsons = []
    def on_resp(r):
        ct = r.headers.get("content-type", "")
        if "json" in ct and len(jsons) < 40:
            try:
                rq = r.request
                jsons.append({"url": r.url, "status": r.status, "method": rq.method,
                              "req_headers": {k: v for k, v in rq.headers.items() if k.lower() not in ("cookie",)},
                              "post": (rq.post_data or "")[:5000], "body": r.text()[:300000]})
            except Exception:
                pass
    page.on("response", on_resp)
    status, err = None, ""
    try:
        resp = page.goto(url, wait_until="domcontentloaded", timeout=45000)
        status = resp.status if resp else None
        try:
            page.wait_for_load_state("networkidle", timeout=8000)
        except Exception:
            pass
        # dismiss cookie banners best-effort, scroll to trigger lazy loads
        for sel in ["text=Accepter alle", "text=Tillad alle", "text=Accept all", "#CybotCookiebotDialogBodyLevelButtonLevelOptinAllowAll"]:
            try:
                page.click(sel, timeout=1500); break
            except Exception:
                pass
        page.mouse.wheel(0, 20000); page.wait_for_timeout(2500)
        html = page.content()
    except Exception as e:
        html, err = "", str(e)[:300]
    page.remove_listener("response", on_resp)
    (d / f"{tag}.html").write_text(html[:1500000])
    (d / f"{tag}_json.json").write_text(json.dumps(jsons, ensure_ascii=False, indent=1))
    links = []
    try:
        for a in page.query_selector_all("a[href]"):
            href = a.get_attribute("href") or ""
            txt = (a.inner_text() or "").strip()[:80]
            full = urllib.parse.urljoin(page.url, href)
            if KW.search(href) or KW.search(txt):
                links.append((full, txt))
    except Exception:
        pass
    links = list(dict.fromkeys(links))
    (d / f"{tag}_links.tsv").write_text("\n".join(f"{u}\t{t}" for u, t in links))
    return status, page.url, len(html), len(jsons), links, err

OUT.mkdir(exist_ok=True)
summary = []
with sync_playwright() as p:
    b = p.chromium.launch()
    ctx = b.new_context(user_agent=UA, locale="da-DK")
    for name, url in sites:
        if only and name not in only:
            continue
        d = OUT / name; d.mkdir(parents=True, exist_ok=True)
        (d / "robots.txt").write_text(robots(url))
        page = ctx.new_page()
        st, final, n, nj, links, err = visit(page, url, d, "p0")
        summary.append([name, url, final, st, n, nj, len(links), err])
        # follow up to 3 listing-looking links on same site one level deep
        host = urllib.parse.urlparse(final).netloc.replace("www.", "")
        cand = [] if NOFOLLOW else [u for u, t in links if re.search(r"ledig|find-?bolig|lejeboliger|ledige|bolig/?$|boliger/?$", u, re.I)
                and u.rstrip("/") != final.rstrip("/")][:2]
        for i, u in enumerate(cand, 1):
            st2, f2, n2, nj2, l2, e2 = visit(page, u, d, f"p{i}")
            summary.append([f"{name}#{i}", u, f2, st2, n2, nj2, len(l2), e2])
        page.close()
        print(name, st, final, n, nj, len(links), err, flush=True)
        (d / "done").write_text("1")
    b.close()
with open(OUT / f"summary_{shard[0].replace('/','of') if shard else 'all'}.tsv", "w") as f:
    f.write("name\turl\tfinal\tstatus\thtml_bytes\tn_json\tn_links\terr\n")
    for r in summary:
        f.write("\t".join(str(x) for x in r) + "\n")
