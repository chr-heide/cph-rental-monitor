"""Dev: plain HTTP to DAB API with different headers, to see what the server needs."""
import json, pathlib, urllib.request, urllib.error
OUT = pathlib.Path("probe_out/bapi"); OUT.mkdir(parents=True, exist_ok=True)
UA = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0 Safari/537.36"
URL = "https://dab-lejerbo.dk/api/memberportal/availabletenancies/getAllAvailableTenancies"
BODY = json.dumps({"selectedNumberOfRooms":0,"selectedMinimumSize":0,"selectedMaxRent":0,"selectedMainRegion":"","selectedRegion":"","fallbackDescriptionToDepartmentDescription":False,"pageNumber":1,"pageSize":10,"shuffleSeed":"x","sortBy":"moveInDate"}).encode()
variants = {
 "json_only": {"Accept": "application/json", "Content-Type": "application/json"},
 "angular": {"Accept": "application/json, text/plain, */*", "Content-Type": "application/json", "Origin": "https://dab-lejerbo.dk", "Referer": "https://dab-lejerbo.dk/"},
 "angular_lang": {"Accept": "application/json, text/plain, */*", "Content-Type": "application/json;charset=UTF-8", "Origin": "https://dab-lejerbo.dk", "Referer": "https://dab-lejerbo.dk/", "Accept-Language": "da-DK,da;q=0.9", "Sec-Fetch-Site": "same-origin", "Sec-Fetch-Mode": "cors", "Sec-Fetch-Dest": "empty"},
}
for name, h in variants.items():
    h = dict(h, **{"User-Agent": UA})
    try:
        r = urllib.request.urlopen(urllib.request.Request(URL, data=BODY, headers=h, method="POST"), timeout=30); st, t = r.status, r.read().decode()
    except urllib.error.HTTPError as e: st, t = e.code, e.read().decode("utf-8","replace")
    except Exception as e: st, t = None, str(e)
    (OUT / f"dabh_{name}.txt").write_text(f"STATUS {st}\n{t[:3000]}"); print(name, st, t[:80].replace("\n"," "))
