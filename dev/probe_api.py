"""Dev: direct HTTP tests from the runner (no browser). Saves results to probe_out/api/."""
import json, pathlib, urllib.request, urllib.error
OUT = pathlib.Path("probe_out/api"); OUT.mkdir(parents=True, exist_ok=True)
UA = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0 Safari/537.36"
def call(name, url, body=None, headers=None):
    h = {"User-Agent": UA, "Accept": "application/json, text/html;q=0.9", "Accept-Language": "da-DK,da;q=0.9"}
    h.update(headers or {})
    data = None
    if body is not None:
        data = json.dumps(body).encode(); h["Content-Type"] = "application/json"
    try:
        r = urllib.request.urlopen(urllib.request.Request(url, data=data, headers=h), timeout=40)
        st, txt = r.status, r.read().decode("utf-8", "replace")
    except urllib.error.HTTPError as e:
        st, txt = e.code, e.read().decode("utf-8", "replace")
    except Exception as e:
        st, txt = None, str(e)
    (OUT / f"{name}.txt").write_text(f"STATUS {st}\nURL {url}\nBODY {json.dumps(body)}\n\n{txt[:400000]}")
    print(name, st, len(txt), flush=True)

FB = "https://findbolig.nu/api/search"
base = {"pageSize": 100, "page": 0, "orderBy": "Created", "orderDirection": "DESC", "mixedResults": False}
for nm, f in {"none": {}, "commune": {"Commune": ["København"]}, "commune_lc": {"commune": ["København"]},
              "postal": {"PostalCode": ["2200"]}, "postal_int": {"PostalCode": [2200]},
              "rentmodel": {"RentModel": ["Advert"]}, "apptype": {"ApplicationType": ["Regular"]},
              "type_res": {"$type": ["Residence"]}}.items():
    call(f"findbolig_{nm}", FB, dict(base, filters=f))
call("findbolig_mode_res", FB, dict(base, filters={}, mode="Residences"))
call("findbolig_mixed_true", FB, dict(base, filters={}, mixedResults=True))
call("juli_no_eid", "https://juliliving.dk/wp-admin/admin-ajax.php?action=jlet_units&locale=da")
call("dab_all", "https://dab-lejerbo.dk/api/memberportal/availabletenancies/getAllAvailableTenancies",
     {"selectedNumberOfRooms": 0, "selectedMinimumSize": 0, "selectedMaxRent": 0, "selectedMainRegion": "", "selectedRegion": "",
      "fallbackDescriptionToDepartmentDescription": False, "pageNumber": 1, "pageSize": 500, "shuffleSeed": "x", "sortBy": "moveInDate"})
call("dab_config", "https://dab-lejerbo.dk/api/memberportal/availabletenancies/getAvailableTenanciesConfiguration")
call("danica_tilleje", "https://bolig.danicaejendomme.dk/api/admin/Property/list/",
     {"page": {"PageNo": 1, "PageSize": 200, "TotalRecords": 0, "TotalPages": 0, "Pages": [], "SortOrder": "", "IsAsc": True, "SearchText": "",
               "Criteria": {"SearchText": "", "SearchPType": "Lejebolig", "SearchPArea": ""}}, "pStatus": "Til leje", "pType": "", "pArea": ""})
call("jeudan", "https://nova-api.jeudan.dk/api/leases/search?lease_type=bolig&per_page=100")
call("cej_list", "https://udlejning.cej.dk/find-bolig")
call("cej_robots", "https://udlejning.cej.dk/robots.txt")
call("bdk_front", "https://www.bdk.dk/umbraco/api/AvailableTenancy/GetTenanciesFrontPage")
call("wanamove_products", "https://wanamove.com/json/products", {"field": "id", "id": "325,324,323,321,320,319,278,227"})
