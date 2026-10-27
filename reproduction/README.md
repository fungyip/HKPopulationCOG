# Reproduction

This directory contains a faithful, additive reproduction of the original
Hong Kong Population Centre of Gravity calculation, plus evidence of its data
coverage and methodological limitations.

## What is reproduced

The legacy arithmetic from `R/Hong Kong Population Center of Gravity.R`:

```
centre_lon = sum(lon * Population) / sum(Population)
centre_lat = sum(lat * Population) / sum(Population)
```

computed in WGS84 geographic degrees, using the **preserved cached coordinates**
in `DataOut/COG_2011.xlsx` and `DataOut/COG_2001.xlsx`.

This is an **algorithmic reproduction using preserved inputs**, not a
restoration of the original R execution environment (no R runtime is available
on the machine used for this work).

## Hard constraint: no re-geocoding

The original analysis geocoded constituency-area names via `ggmap::geocode`.
Re-running a geocoder today would return different coordinates and change an
input, which would make it impossible to distinguish a reproduction from a
revised analysis. The reproduction therefore reads coordinates only from the
cached `DataOut/COG_*.xlsx` files.

## How to run

```bash
python reproduction/scripts/reproduce_legacy.py
```

Requirements: Python 3.12 with `pandas` and `openpyxl`.

The script writes:

- `reports/historical-centres.csv`
- `reports/coverage-summary.csv`

and prints the same values to stdout.

## Layout

```
reproduction/
  README.md
  scripts/
    reproduce_legacy.py
  reports/
    legacy-reproduction.md
    historical-centres.csv
    coverage-summary.csv
```

## Status

See `reports/legacy-reproduction.md` for the per-finding reproduction status
and `../docs/methodology-review.md` (future) for the audit of limitations.
