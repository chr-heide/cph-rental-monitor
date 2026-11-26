"""Dev: can CEJ be read with an honest, identifiable user agent? Two requests, 5 s apart."""
import pathlib, urllib.request, urllib.error, time, re
OUT = pathlib.Path("probe_out"); OUT.mkdir(exist_ok=True)
UA = "cph-rental-monitor/1.0 (+https://github.com/chr-heide/cph-rental-monitor; private apartment alert, max every 10 min)"
log = []
for name, url in [("list", "https://udlejning.cej.dk/find-bolig"), ("robots", "https://udlejning.cej.dk/robots.txt")]:
    req = urllib.request.Request(url, headers={"User-Agent": UA, "Accept": "text/html,application/json", "Accept-Language": "da-DK,da;q=0.9"})
    try:
        r = urllib.request.urlopen(req, timeout=30); st, t = r.status, r.read().decode("utf-8", "replace")
    except urllib.error.HTTPError as e: st, t = e.code, e.read().decode("utf-8", "replace")
    except Exception as e: st, t = None, str(e)
    (OUT / f"cej_{name}.txt").write_text(t[:400000])
    log.append(f"{name} {st} {len(t)} boliger_links={len(set(re.findall(r'/boliger/[^\"<> ]+', t)))} blocked={'blockedRequest' in t or 'Too Many' in t}")
    time.sleep(5)
(OUT / "cej_log.txt").write_text("\n".join(log)); print("\n".join(log))
