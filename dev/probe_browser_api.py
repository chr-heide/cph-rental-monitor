"""Dev: run fetch() calls from inside a real page (handles TLS chains / CORS like a browser)."""
import json, pathlib
from playwright.sync_api import sync_playwright
OUT = pathlib.Path("probe_out/bapi"); OUT.mkdir(parents=True, exist_ok=True)
UA = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0 Safari/537.36"
JS = """async ([url, body]) => { const o = body ? {method:'POST', headers:{'Content-Type':'application/json'}, body: JSON.stringify(body)} : {};
  try { const r = await fetch(url, o); return {status: r.status, text: (await r.text()).slice(0, 300000)}; } catch (e) { return {status: -1, text: String(e)}; } }"""
NYB = json.loads('{"siteName":"nybolig","municipalityCodes":[],"municipalityNames":[],"cityNames":[],"supplementaryCityNames":[],"postalCodes":[],"postalCodeRanges":[],"streetNames":[],"streetNumbers":[],"propertyCategories":[],"businessPropertyCategories":[],"isRental":true,"isSold":false,"isSaleInProgress":false,"polygon":[],"freeText":"","sort":0,"scrollToken":"","top":20}')
def nyb(**kw):
    d = dict(NYB); d.update(kw); return d
DAB = {"selectedNumberOfRooms":0,"selectedMinimumSize":0,"selectedMaxRent":0,"selectedMainRegion":"","selectedRegion":"","fallbackDescriptionToDepartmentDescription":False,"pageNumber":1,"pageSize":10,"shuffleSeed":"b5m3d1c","sortBy":"moveInDate"}
tests = {
 "https://www.nybolig.dk/soegeresultat-boliger?isRental=true": [
   ("nyb_plain", "https://www.nybolig.dk/api/search/cases/find", nyb()),
   ("nyb_muni", "https://www.nybolig.dk/api/search/cases/find", nyb(municipalityNames=["København"])),
   ("nyb_muni_code", "https://www.nybolig.dk/api/search/cases/find", nyb(municipalityCodes=[101])),
   ("nyb_pc", "https://www.nybolig.dk/api/search/cases/find", nyb(postalCodes=[2200, 2100])),
   ("nyb_pcr", "https://www.nybolig.dk/api/search/cases/find", nyb(postalCodeRanges=["1000-2450"])),
   ("nyb_pcr2", "https://www.nybolig.dk/api/search/cases/find", nyb(postalCodeRanges=[{"from":1000,"to":2450}])),
   ("nyb_pcr3", "https://www.nybolig.dk/api/search/cases/find", nyb(postalCodeRanges=[{"min":1000,"max":2450}])),
 ],
 "https://dab-lejerbo.dk/": [
   ("dab_browser", "https://dab-lejerbo.dk/api/memberportal/availabletenancies/getAllAvailableTenancies", DAB),
 ],
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
    b.close()
