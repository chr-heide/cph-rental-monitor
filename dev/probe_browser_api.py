"""Dev: run fetch() calls from inside a real page (handles TLS chains / CORS like a browser)."""
import json, pathlib
from playwright.sync_api import sync_playwright
OUT = pathlib.Path("probe_out/bapi"); OUT.mkdir(parents=True, exist_ok=True)
UA = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0 Safari/537.36"
JS = """async ([url, body]) => { const o = body ? {method:'POST', headers:{'Content-Type':'application/json'}, body: JSON.stringify(body)} : {};
  try { const r = await fetch(url, o); return {status: r.status, text: (await r.text()).slice(0, 300000)}; } catch (e) { return {status: -1, text: String(e)}; } }"""
tests = {
 "https://findbolig.nu/da-dk": [
   ("fb_robots", "https://findbolig.nu/robots.txt", None),
   ("fb_none", "https://findbolig.nu/api/search", {"pageSize":100,"page":0,"orderBy":"Created","orderDirection":"DESC","mixedResults":False,"filters":{}}),
   ("fb_mixed", "https://findbolig.nu/api/search", {"pageSize":100,"page":0,"orderBy":"Created","orderDirection":"DESC","mixedResults":True,"filters":{}}),
   ("fb_commune", "https://findbolig.nu/api/search", {"pageSize":20,"page":0,"orderBy":"Created","orderDirection":"DESC","mixedResults":True,"filters":{"Commune":["København"]}}),
   ("fb_postal", "https://findbolig.nu/api/search", {"pageSize":20,"page":0,"orderBy":"Created","orderDirection":"DESC","mixedResults":True,"filters":{"PostalCode":["2200"]}}),
   ("fb_rentmodel", "https://findbolig.nu/api/search", {"pageSize":20,"page":0,"orderBy":"Created","orderDirection":"DESC","mixedResults":True,"filters":{"RentModel":["Advert"]}}),
   ("fb_apptype", "https://findbolig.nu/api/search", {"pageSize":20,"page":0,"orderBy":"Created","orderDirection":"DESC","mixedResults":True,"filters":{"ApplicationType":["Regular"]}}),
   ("fb_area", "https://findbolig.nu/api/search", {"pageSize":20,"page":0,"orderBy":"Created","orderDirection":"DESC","mixedResults":True,"filters":{"Area":[50,500],"Rooms":[2,10],"Rent":[0,16000]}}),
 ],
 "https://www.balder.dk/lejeboliger": [("balder_api_robots", "https://api.balder.dk/robots.txt", None)],
}
with sync_playwright() as p:
    b = p.chromium.launch(); ctx = b.new_context(user_agent=UA, locale="da-DK")
    for page_url, calls in tests.items():
        pg = ctx.new_page()
        try: pg.goto(page_url, wait_until="domcontentloaded", timeout=45000)
        except Exception as e: print("goto fail", page_url, e)
        for name, url, body in calls:
            r = pg.evaluate(JS, [url, body])
            (OUT / f"{name}.txt").write_text(f"STATUS {r['status']}\nURL {url}\nBODY {json.dumps(body)}\n\n{r['text']}")
            print(name, r["status"], len(r["text"]), flush=True)
        pg.close()
    # Go'Bolig: record every request to its API gateway, whatever the content type
    pg = ctx.new_page(); seen = []
    pg.on("response", lambda r: seen.append(r) if "gobolig-api" in r.url and "/images/" not in r.url else None)
    pg.goto("https://gobolig.dk/soeg-lejebolig/", wait_until="networkidle", timeout=60000)
    pg.mouse.wheel(0, 30000); pg.wait_for_timeout(4000)
    out = []
    for r in seen:
        try: out.append({"url": r.url, "method": r.request.method, "post": r.request.post_data, "status": r.status, "body": r.text()[:200000]})
        except Exception as e: out.append({"url": r.url, "err": str(e)})
    (OUT / "gobolig_api.json").write_text(json.dumps(out, ensure_ascii=False, indent=1)); print("gobolig", len(out))
    b.close()
