"""Compare legacy and revised methods for the HK population centre of gravity.

Methods
-------
legacy
    Weighted mean of (lon, lat) in WGS84 degrees. Reproduces the original.
projected
    Weighted mean computed in Hong Kong 1980 Grid (EPSG:2326), then transformed
    back to WGS84. Isolates the effect of the coordinate system only.
projected_coverage_corrected
    Projected method with the known geocoding error removed ("North - Yan Do",
    2001, cached in Sichuan, China). Quantifies coverage sensitivity.

The revised, maintained implementation lives in HKGeo; this script only
produces the controlled comparison for the reproduction evidence trail.

Inputs
------
    DataOut/COG_2011.xlsx, DataOut/COG_2001.xlsx  cached geocoded coordinates
"""

import csv
import math
from pathlib import Path

import pandas as pd
from pyproj import Transformer

REPO_ROOT = Path(__file__).resolve().parents[2]
DATA_OUT = REPO_ROOT / "DataOut"
REPORTS = REPO_ROOT / "reproduction" / "reports"

EARTH_RADIUS_M = 6371000.0

WGS84_TO_HK80 = Transformer.from_crs("EPSG:4326", "EPSG:2326", always_xy=True)
HK80_TO_WGS84 = Transformer.from_crs("EPSG:2326", "EPSG:4326", always_xy=True)

BAD_2001_AREA = "North - Yan Do"


def read_cached(path):
    frame = pd.read_excel(path, sheet_name="Sheet1")
    frame = frame[["Area", "Population", "lon", "lat"]].copy()
    frame["Population"] = pd.to_numeric(frame["Population"], errors="coerce")
    frame["lon"] = pd.to_numeric(frame["lon"], errors="coerce")
    frame["lat"] = pd.to_numeric(frame["lat"], errors="coerce")
    return frame.dropna(subset=["Population", "lon", "lat"]).reset_index(drop=True)


def haversine_m(lon1, lat1, lon2, lat2):
    phi1 = math.radians(lat1)
    phi2 = math.radians(lat2)
    dphi = math.radians(lat2 - lat1)
    dlmb = math.radians(lon2 - lon1)
    a = math.sin(dphi / 2) ** 2 + math.cos(phi1) * math.cos(phi2) * math.sin(dlmb / 2) ** 2
    return EARTH_RADIUS_M * 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))


def initial_bearing(lon1, lat1, lon2, lat2):
    phi1 = math.radians(lat1)
    phi2 = math.radians(lat2)
    dlmb = math.radians(lon2 - lon1)
    x = math.sin(dlmb) * math.cos(phi2)
    y = math.cos(phi1) * math.sin(phi2) - math.sin(phi1) * math.cos(phi2) * math.cos(dlmb)
    return (math.degrees(math.atan2(x, y)) + 360.0) % 360.0


def degree_centre(frame):
    population = float(frame["Population"].sum())
    lon = float((frame["lon"] * frame["Population"]).sum() / population)
    lat = float((frame["lat"] * frame["Population"]).sum() / population)
    return population, lon, lat


def projected_centre(frame):
    easting, northing = WGS84_TO_HK80.transform(
        frame["lon"].to_numpy(), frame["lat"].to_numpy()
    )
    population = float(frame["Population"].sum())
    mean_e = float((easting * frame["Population"].to_numpy()).sum() / population)
    mean_n = float((northing * frame["Population"].to_numpy()).sum() / population)
    lon, lat = HK80_TO_WGS84.transform(mean_e, mean_n)
    return population, float(lon), float(lat)


def main():
    cached = {
        "2011": read_cached(DATA_OUT / "COG_2011.xlsx"),
        "2001": read_cached(DATA_OUT / "COG_2001.xlsx"),
    }

    legacy = {}
    for period in ("2011", "2001"):
        legacy[period] = degree_centre(cached[period])

    rows = []
    for period in ("2011", "2001"):
        frame = cached[period]

        pop, lon, lat = degree_centre(frame)
        legacy_lon, legacy_lat = lon, lat
        rows.append({
            "reference_period": period,
            "method_id": "legacy",
            "included_population": int(pop),
            "excluded_population": 0,
            "centre_longitude": round(lon, 6),
            "centre_latitude": round(lat, 6),
            "difference_from_legacy_m": 0.0,
            "notes": "WGS84 degree average; reproduces original",
        })

        pop, lon, lat = projected_centre(frame)
        rows.append({
            "reference_period": period,
            "method_id": "projected",
            "included_population": int(pop),
            "excluded_population": 0,
            "centre_longitude": round(lon, 6),
            "centre_latitude": round(lat, 6),
            "difference_from_legacy_m": round(haversine_m(legacy_lon, legacy_lat, lon, lat), 1),
            "notes": "HK 1980 Grid (EPSG:2326) mean centre",
        })

        corrected = frame
        notes = "projected + coverage correction"
        if period == "2001":
            corrected = frame[frame["Area"] != BAD_2001_AREA].reset_index(drop=True)
            notes = "projected; removed 'North - Yan Do' geocoding error"
        pop, lon, lat = projected_centre(corrected)
        excluded = int(frame["Population"].sum() - corrected["Population"].sum())
        rows.append({
            "reference_period": period,
            "method_id": "projected_coverage_corrected",
            "included_population": int(pop),
            "excluded_population": excluded,
            "centre_longitude": round(lon, 6),
            "centre_latitude": round(lat, 6),
            "difference_from_legacy_m": round(haversine_m(legacy_lon, legacy_lat, lon, lat), 1),
            "notes": notes,
        })

    with open(REPORTS / "method-comparison.csv", "w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)

    for row in rows:
        print(row)

    print()
    print("2001 -> 2011 shift by method")
    for method in ("legacy", "projected", "projected_coverage_corrected"):
        m = {r["reference_period"]: r for r in rows if r["method_id"] == method}
        lon01 = m["2001"]["centre_longitude"]
        lat01 = m["2001"]["centre_latitude"]
        lon11 = m["2011"]["centre_longitude"]
        lat11 = m["2011"]["centre_latitude"]
        distance = haversine_m(lon01, lat01, lon11, lat11)
        bearing = initial_bearing(lon01, lat01, lon11, lat11)
        print(f"{method:32s} {distance:9.1f} m  bearing {bearing:6.1f} deg")


if __name__ == "__main__":
    main()
