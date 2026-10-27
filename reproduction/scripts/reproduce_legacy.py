"""Algorithmic reproduction of the legacy HK population centre-of-gravity calculation.

This script reproduces the arithmetic from the original R analysis using the
preserved cached inputs. It is NOT a restoration of the original R execution
environment (no R runtime is available), and it deliberately does NOT re-geocode
area names, because new geocoder responses would change an input and obscure the
distinction between reproduction and a revised analysis.

Legacy arithmetic (as in R/Hong Kong Population Center of Gravity.R):
    centre_lon = sum(lon * Population) / sum(Population)
    centre_lat = sum(lat * Population) / sum(Population)
computed in WGS84 geographic degrees.

Inputs:
    DataIn/table_2011.xlsx, DataIn/table_2001.xlsx   census population by area
    DataOut/COG_2011.xlsx, DataOut/COG_2001.xlsx     cached geocoded coordinates

Outputs (written to reproduction/reports/):
    historical-centres.csv   computed centres, distance and bearing
    coverage-summary.csv     included/excluded area and population counts
"""

import csv
import math
import os
from pathlib import Path

import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[2]
DATA_IN = REPO_ROOT / "DataIn"
DATA_OUT = REPO_ROOT / "DataOut"
REPORTS = REPO_ROOT / "reproduction" / "reports"

EARTH_RADIUS_M = 6371000.0


def read_census(path):
    frame = pd.read_excel(path, sheet_name="Sheet1", header=None, skiprows=3)
    frame.columns = ["Area", "Population"]
    frame = frame[pd.notna(frame["Population"])].copy()
    frame["Population"] = pd.to_numeric(frame["Population"], errors="coerce")
    frame = frame[pd.notna(frame["Population"])].copy()
    total = frame.loc[frame["Area"] == "Total", "Population"].iloc[0]
    areas = frame[frame["Area"] != "Total"].reset_index(drop=True)
    return areas, int(total)


def read_cached(path):
    frame = pd.read_excel(path, sheet_name="Sheet1")
    frame = frame[["Area", "Population", "lon", "lat"]].copy()
    frame["Population"] = pd.to_numeric(frame["Population"], errors="coerce")
    frame["lon"] = pd.to_numeric(frame["lon"], errors="coerce")
    frame["lat"] = pd.to_numeric(frame["lat"], errors="coerce")
    return frame.dropna(subset=["Population", "lon", "lat"]).reset_index(drop=True)


def weighted_centre(frame):
    population = frame["Population"].sum()
    lon = (frame["lon"] * frame["Population"]).sum() / population
    lat = (frame["lat"] * frame["Population"]).sum() / population
    return population, lon, lat


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


def count_shared_coordinates(frame):
    rounded = list(zip(frame["lon"].round(5), frame["lat"].round(5)))
    counts = {}
    for key in rounded:
        counts[key] = counts.get(key, 0) + 1
    unique_pairs = len(counts)
    duplicated_pairs = sum(1 for count in counts.values() if count > 1)
    areas_sharing = sum(count for count in counts.values() if count > 1)
    return unique_pairs, duplicated_pairs, areas_sharing


def main():
    REPORTS.mkdir(parents=True, exist_ok=True)

    census_2011, census_2011_total = read_census(DATA_IN / "table_2011.xlsx")
    census_2001, census_2001_total = read_census(DATA_IN / "table_2001.xlsx")

    cached = {
        "2011": read_cached(DATA_OUT / "COG_2011.xlsx"),
        "2001": read_cached(DATA_OUT / "COG_2001.xlsx"),
    }

    census = {
        "2011": (census_2011, census_2011_total),
        "2001": (census_2001, census_2001_total),
    }

    centres = {}
    coverage_rows = []

    for period in ("2011", "2001"):
        analysed = cached[period]
        census_areas, census_total = census[period]
        population, lon, lat = weighted_centre(analysed)
        centres[period] = (lon, lat)

        analysed_areas = set(analysed["Area"])
        census_area_names = set(census_areas["Area"])
        missing = sorted(census_area_names - analysed_areas)
        missing_population = int(census_areas.loc[census_areas["Area"].isin(missing), "Population"].sum())

        unique_coords, duplicated_pairs, areas_sharing = count_shared_coordinates(analysed)

        coverage_rows.append({
            "reference_period": period,
            "census_areas": int(len(census_areas)),
            "analysed_areas": int(len(analysed)),
            "missing_areas": "; ".join(missing),
            "census_population": census_total,
            "analysed_population": int(population),
            "excluded_population": missing_population,
            "unique_coordinate_pairs": unique_coords,
            "duplicated_coordinate_pairs": duplicated_pairs,
            "areas_on_duplicated_coordinates": areas_sharing,
        })

    lon_2011, lat_2011 = centres["2011"]
    lon_2001, lat_2001 = centres["2001"]
    distance_m = haversine_m(lon_2011, lat_2011, lon_2001, lat_2001)
    bearing = initial_bearing(lon_2001, lat_2001, lon_2011, lat_2011)

    centre_rows = [
        {
            "reference_period": "2011",
            "method_id": "legacy",
            "centre_longitude": float(round(lon_2011, 6)),
            "centre_latitude": float(round(lat_2011, 6)),
            "distance_from_legacy_m": "",
            "bearing_from_2001_deg": "",
        },
        {
            "reference_period": "2001",
            "method_id": "legacy",
            "centre_longitude": float(round(lon_2001, 6)),
            "centre_latitude": float(round(lat_2001, 6)),
            "distance_from_legacy_m": "",
            "bearing_from_2001_deg": "",
        },
        {
            "reference_period": "2011",
            "method_id": "legacy_shift",
            "centre_longitude": float(round(lon_2011, 6)),
            "centre_latitude": float(round(lat_2011, 6)),
            "distance_from_legacy_m": float(round(distance_m, 1)),
            "bearing_from_2001_deg": float(round(bearing, 1)),
        },
    ]

    with open(REPORTS / "historical-centres.csv", "w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(centre_rows[0].keys()))
        writer.writeheader()
        writer.writerows(centre_rows)

    with open(REPORTS / "coverage-summary.csv", "w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(coverage_rows[0].keys()))
        writer.writeheader()
        writer.writerows(coverage_rows)

    print("historical-centres.csv")
    for row in centre_rows:
        print(row)
    print()
    print("coverage-summary.csv")
    for row in coverage_rows:
        print(row)


if __name__ == "__main__":
    main()
