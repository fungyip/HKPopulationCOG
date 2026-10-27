# Hong Kong Population Centre of Gravity

## Project status

This repository preserves the original 2016 analysis of the Hong Kong
population centre of gravity using 2001 and 2011 census data. Its historical
results have been reproduced and methodologically reviewed. The original
outputs are frozen as-is; the maintained, interactive successor lives in
[HKGeo](https://github.com/fungyip/HKGeo).

## Original research question

- Where is the HK population centre of gravity?
- Where is it shifting?

## Original data and method

The original analysis used Hong Kong census data (2001 and 2011), geocoded each
District Council Constituency Area by name, multiplied population by longitude
and latitude independently, and divided the coordinate sums by total population
to obtain a population-weighted centre. A 2015 boundary shapefile was used to
label the resulting point with a district and area.

The original R script is preserved at
[`R/Hong Kong Population Center of Gravity.R`](R/Hong Kong Population Center of Gravity.R).

## Historical results and validation status

| Historical finding | Status |
|---|---|
| 2011 centre at (22.35215, 114.1501) | **Not reproduced as stated** — committed data gives (114.151562, 22.353321) |
| 2001 centre at (22.38664, 114.1116) | **Not reproduced exactly** — committed data gives (114.114203, 22.387307) |
| Shift of ~5,500 m southward | **Superseded by a documented correction** — see below |

The published "~5,500 m southward" shift is largely an artifact of a single
geocoding error: the 2001 area "North - Yan Do" was cached in Sichuan, mainland
China. Removing it moves the 2001 centre to (114.1562, 22.3528), leaving a
shift of only **~481 m** (bearing ~278°, essentially due west) to the 2011
centre.

![Historical map](DataOut/hk_COG.png "HK Map Pop COG (original)")

## Reproduction instructions

The legacy calculation is reproduced in Python (the original R environment is
not recoverable on current hardware):

```bash
python reproduction/scripts/reproduce_legacy.py
python reproduction/scripts/compare_methods.py
python -m unittest discover -s tests  # (in HKGeo)
```

Requirements: Python 3.12 with `pandas`, `openpyxl`, `pyproj` (`geopandas`
for HKGeo).

See [`reproduction/`](reproduction/) for the scripts and reports.

## Data coverage and limitations

- 2011: all 412 areas, population 7,070,388 (matches census).
- 2001: 389 of 390 areas — "Islands - Cheung Chau North" (12,288) was dropped
  by a fixed row range.
- Name-geocoded locations are not polygon centroids (2011: 342 unique coordinate
  pairs for 412 areas; 2001: 331 for 389), and include one gross error.
- Labels use 2015 boundaries against 2001/2011 points.
- Mean centres were computed in WGS84 degrees, not projected coordinates
  (negligible at HK scale — ~2.5 m).

Full audit: [`docs/methodology-review.md`](docs/methodology-review.md).
Inventory: [`docs/data-inventory.csv`](docs/data-inventory.csv).
Provenance: [`docs/provenance.md`](docs/provenance.md).

## Differences in the revised implementation

The revised method (in HKGeo) validates and cleans locations, projects to
Hong Kong 1980 Grid (EPSG:2326), computes the weighted mean centre in projected
coordinates, then transforms back to WGS84:

| centre | longitude | latitude |
|---|---|---|
| 2011 (corrected) | 114.151575 | 22.353339 |
| 2001 (error removed) | 114.156205 | 22.352752 |

Corrected 2001 → 2011 shift: **480.6 m at bearing 277.8°**.

## Interactive successor: HKGeo

The modern analytical implementation and interactive Leaflet map are in
[HKGeo](https://github.com/fungyip/HKGeo). See its
[handover record](https://github.com/fungyip/HKGeo/blob/master/docs/legacy-provenance.md).

## Sources and attribution

- Census and Statistics Department, Hong Kong SAR — 2001 and 2011 population by
  District Council / Constituency Area (Interactive Data Dissemination Service).
- Hong Kong Government — DC 2015 constituency area boundary shapefile
  (`GIH3_DC_2015_POLY`, HK 1980 Grid).
- Original analysis by Fung YIP, October 2016.

> The original write-up, R scripts, and code remain available in this
> repository and its Git history. Historical outputs should be interpreted in
> the context of their documented data and methods.
