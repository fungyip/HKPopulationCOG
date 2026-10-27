# Provenance

## Repository identity

- Repository: `fungyip/HKPopulationCOG`
- Remote: `https://github.com/fungyip/HKPopulationCOG`
- Default branch: `master`
- Author: Fung YIP

## Commit captured at reproduction

- HEAD: `bb580938cc5ce54186759c42155655b9cc59c092` ("Delete RWordPress.R")
- Captured on branch: `docs/reproduction-and-integration`

## Commit history (default branch)

| Commit | Message |
|---|---|
| `995f40a` | Initial commit |
| `7c6fa45` | Initial Upload |
| `1f45699` | Update Readme |
| `088832a` | Update |
| `bb58093` | Delete RWordPress.R |

## Reproduction discipline

The reproduction reads population from `DataIn/table_*.xlsx` and cached
geocoded coordinates from `DataOut/COG_*.xlsx`. It **does not** call a geocoder,
because new geocoder responses would change an input and obscure the difference
between reproduction and revision.

## Reproduction environment

- Python 3.12.9
- pandas 3.0.3
- openpyxl 3.1.5
- No R runtime was available; the original R script was not re-executed.

## Known provenance gaps

- Census tables carry no explicit licence statement; they originate from the
  Census and Statistics Department Interactive Data Dissemination Service
  (generated 7/5/2016).
- The `DC_2015` boundary shapefile originates from the Hong Kong Government
  (`GOVHK.GIH3_DC_2015_POLY`) but its redistribution licence is unrecorded.
