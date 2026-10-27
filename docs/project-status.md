# Project status

> This repository preserves the original Hong Kong Population Centre of Gravity
> analysis using 2001 and 2011 census data. Its historical results are
> undergoing reproduction and methodological review. An updated interactive
> application is planned in HKGeo. Original outputs should be interpreted in the
> context of their documented data and methods.

## What this repository is

- The original research and its historical outputs (`R/`, `DataOut/`, `Markdown/`).
- Reproduction evidence (`reproduction/`).
- Documented methodological limitations (`docs/`).

## What this repository is not

- A second, competing web application.
- A maintained publishing pipeline. The interactive successor lives in **HKGeo**
  (Python analysis + static Leaflet/GeoJSON HTML output).

## Navigation

| Path | Purpose |
|---|---|
| `R/` | Original R scripts (historical, not re-executed here) |
| `DataIn/` | Census population workbooks and boundary shapefile |
| `DataOut/` | Historical geocoded outputs, weighted coordinates, map, POI |
| `Markdown/` | Original write-up (Rmd/md/html) |
| `reproduction/` | Python reproduction of the legacy calculation + coverage evidence |
| `docs/` | Provenance, inventory, and (future) methodology review |

## Planned successor

The modern analytical implementation and interactive HTML application are being
built in `fungyip/HKGeo`, which will document its handover in
`docs/legacy-provenance.md`.

## Historical vs. revised

Distinguish the two clearly: historical findings in this repository are frozen
as-is (see `reproduction/reports/legacy-reproduction.md` for their reproduction
status); revised, projected-coordinate results will be produced and maintained
in HKGeo only.
