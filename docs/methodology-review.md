# Methodology review

A review of the original analysis's methodological limitations, separated into
**verified limitations**, **potential concerns**, and **resolved issues**. This
documents what materially affects the original result and what does not.

## Headline finding

The original headline result — that the HK population centre moved
**~5,500 m** between 2001 and 2011 — is largely an artifact of a single
geocoding error, not a genuine shift in population distribution.

The 2001 constituency "**North - Yan Do**" (population 27,722, one of the
largest North District constituencies) was geocoded to
**(104.0225°E, 30.6958°N)** — a location in Sichuan province, mainland China,
roughly 1,300 km from Hong Kong. This single point is the only coordinate in
either cached dataset that falls outside the Hong Kong study region.

| 2001 centre | longitude | latitude | shift to 2011 centre |
|---|---|---|---|
| As published (includes the error) | 114.114203 | 22.387307 | 5,388.7 m |
| Excluding "North - Yan Do" | 114.156193 | 22.352736 | **480.7 m** (bearing ~278°) |

When the erroneous point is excluded, the 2001 centre moves to
(114.156193, 22.352736), which is only **~481 m** from the 2011 centre
(114.151562, 22.353321). The reported "southward" movement is therefore
predominantly an artifact. This is an audit-level sensitivity result, not an
authoritative revised centre; the revised projected-coordinate calculation
belongs to Phase 5 / HKGeo.

---

## Verified limitations

These are confirmed against the committed data and materially affect the result.

### 1. Name-geocoded representative locations

- 2011: 412 areas map to only **342 unique coordinate pairs** (38 pairs shared,
  108 areas on a shared coordinate).
- 2001: 389 areas map to only **331 unique coordinate pairs** (27 pairs shared,
  85 areas on a shared coordinate).
- The returned locations are therefore district/island-level approximations,
  **not** polygon centroids. The "centre of gravity" is averaged over these
  shared approximate points.
- One location ("North - Yan Do", 2001) is grossly wrong (Sichuan, China) and
  dominates the historical movement claim (see headline finding).

### 2. Fixed row selection omitted a real area

- `data_2001[1:389,]` dropped the final census row, "**Islands - Cheung Chau
  North**" (population 12,288). The 2001 analysis represents 6,690,206 of the
  6,702,494 census total.
- 2011's `[1:412,]` range is correct: all 412 areas are included.

### 3. Boundary lookup uses a later dataset

- District/area labels were assigned via `over()` against
  `DC_2015_poly` (`GIH3_DC_2015_POLY.shp`, 431 DCAs, HK 1980 Grid, EPSG:2326).
- The boundaries are dated 2015, while the analysis points are from 2001 and
  2011. Any district/area attribution is therefore anachronistic and must be
  labelled as "2015 boundary", not the analysis-year boundary.

### 4. Distance and direction reporting

- Independently recomputed (Haversine): the 2001→2011 distance is
  **5,388.7 m** and the initial bearing is **134.5°** (south-east), not the
  README's "~5,500 m" and "southward".
- More importantly, once the geocoding error is excluded, the movement is
  **~481 m at bearing ~278°** (essentially due west), which contradicts the
  published narrative regardless of the exact wording.

---

## Potential concerns

These may affect the result but require a controlled comparison to quantify.

### 5. Direct latitude/longitude averaging

- The weighted mean is computed in WGS84 **degrees** (`sum(lon·pop)/sum(pop)`,
  `sum(lat·pop)/sum(pop)`), not in a projected coordinate system.
- At Hong Kong's scale the distortion is small (tens of metres), but a mean
  centre should be computed on projected coordinates (HK 1980 Grid, EPSG:2326).
  This is quantified in Phase 5.

### 6. Different reference-period geographies

- The number of District Council Constituency Areas differs across periods:
  390 (2001), 412 (2011), 431 (2015 boundary). Area names and boundaries are
  not stable across these periods, so a strict "centre moved X m" interpretation
  conflates population change with boundary change.

---

## Resolved issues

### 7. Missing-record removal (`na.omit`)

- The original code dropped incomplete records after geocoding. In the
  committed cached data there are **no missing coordinates** in either dataset
  (all 412 and 389 rows are complete), so `na.omit` removed nothing. The 2001
  omission is entirely due to fixed row selection (issue 2), not missing data.

### 8. 2011 coverage

- 2011 represented population is 7,070,388, exactly matching the census total.
  No coverage loss in 2011.

---

## Summary table

| # | Issue | Status | Material? |
|---|---|---|---|
| 1 | Name-geocoded locations | Verified | Yes — including one gross error in 2001 |
| 2 | Fixed row selection | Verified | Yes — 12,288 people omitted in 2001 |
| 3 | Later boundary dataset | Verified | Yes — anachronistic labels |
| 4 | Distance/direction reporting | Verified | Yes — narrative contradicted |
| 5 | Degree vs projected averaging | Potential | Minor; quantify in Phase 5 |
| 6 | Reference-period geography | Potential | Affects interpretation |
| 7 | `na.omit` removal | Resolved | No effect |
| 8 | 2011 coverage | Resolved | No loss |
