"""Dev: save Kereby card markup (2 cards + their parents) and the status labels used."""
import re, pathlib, urllib.request
OUT = pathlib.Path("probe_out"); OUT.mkdir(exist_ok=True)
UA = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0 Safari/537.36"
h = urllib.request.urlopen(urllib.request.Request("https://kereby.dk/bolig/", headers={"User-Agent": UA}), timeout=40).read().decode("utf-8", "replace")
h2 = re.sub(r"<svg.*?</svg>", "<svg/>", h, flags=re.S); h2 = re.sub(r'(srcset|sizes|style)="[^"]*"', "", h2)
starts = [m.start() for m in re.finditer(r'<article[^>]*jorato-case-card', h2)]
snip = "\n\n=========\n\n".join(h2[max(0, s - 800): s + 5000] for s in starts[:2])
(OUT / "kereby_cards.html").write_text(snip)
states = sorted(set(re.findall(r'data-state="([^"]*)"', h2)))
labels = sorted(set(re.sub(r"\s+", " ", x).strip() for x in re.findall(r'jorato-case-card__(?:status|state|badge|label)[^>]*>([^<]{1,40})<', h2)))
hrefs = sorted(set(re.findall(r'href="(https://kereby\.dk/bolig/[^"]+)"', h2)))[:10]
(OUT / "kereby_summary.txt").write_text(f"cards={len(starts)}\nstates={states}\nlabels={labels}\nhrefs={hrefs}\n")
print(open(OUT / "kereby_summary.txt").read())
