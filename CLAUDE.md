# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this package is

`climateData` is an R package that wraps access to [ClimateNA](https://climatena.ca/) outputs for
North America. The package itself does not bundle raster data; instead, `inst/extdata/` contains
a SQLite lookup database and a GeoPackage of tile polygons, and raw raster archives are pulled on
demand from Google Drive. The `data-raw/` scripts (run by the maintainer on a Windows box with the
ClimateNA executable) regenerate those archives and the SQLite index; they are excluded from the
built package via `.Rbuildignore` and should not be treated as user-facing code.

## Common commands

Formatting is enforced by [Air](https://github.com/posit-dev/air) (config in `air.toml`;
line-width 100, 2-space indent). Do not hand-reformat past what Air would produce. VS Code is
configured to format on save via `Posit.air-vscode`.

```r
# build docs + NAMESPACE from roxygen (do this after editing any R/*.R)
devtools::document()

# check the package (mirrors CI in .github/workflows/R-CMD-check.yaml)
devtools::check()

# run the full test suite
devtools::test()

# run a single test file
testthat::test_file("tests/testthat/test-prepClimateLayers.R")

# load all package code into the session (used at top of data-raw/ scripts)
devtools::load_all()
```

Most tests `skip_on_ci()` and `skip_on_cran()` because they require interactive `googledrive`
auth and multi-GB downloads from Google Drive. Expect the CI test run to be effectively empty;
real validation requires running `devtools::test()` locally with credentials in `.secrets/`.

`makeClimateDEM` tests are Windows-only (ClimateNA rejects DEMs written on non-Windows platforms
due to line-ending differences — see `rewrite_asc()`).

## Architecture

### The pipeline: `prepClimateLayers()`

`prepClimateLayers()` in [R/prepClimateLayers.R](R/prepClimateLayers.R) is the main user entry
point and orchestrates the rest of the package. Given a `climateVarsList` describing derived
climate variables, it:

1. Parses variable names via `whichTypes()` / `whichMSYN()` to decide which ClimateNA archives are
   needed. Variable-name convention: `<type>_<VAR>[_normal]` where `type ∈ {historical, future}`;
   suffix disambiguates temporal resolution — `_normal` → N (normals), `_(wt|sp|sm|at)` → S
   (seasonal), two trailing digits → M (monthly), otherwise Y (yearly).
2. Consolidates M/S/Y requests into the combined `MSY` archive when all three are needed for the
   same type (avoids downloading three overlapping zips).
3. Calls `getClimateURLs()` → `getClimateTiles()` to pull archives from Google Drive via
   `reproducible::preProcess()`. URLs are looked up in the bundled SQLite
   (`ClimateNA_tiles_sqlite()`); the schema differs per `type` and is defined in `ClimateNA_sql()`.
4. Calls `buildClimateMosaics()` / `buildClimateMosaicsNormals()` to build GDAL VRTs per tile and
   warp them to per-year/period GeoTIFFs — this is the CPU-heavy step and runs in parallel (see
   backend section).
5. Wraps each GeoTIFF set into a `SpatRaster` stack via `climateStacksByYear()` /
   `climateStacksByPeriod()`.
6. Evaluates the user-supplied `fun` in each `climateVarsList` entry against those stacks. The
   `calc*` family in [R/calcVars.R](R/calcVars.R) is the built-in set: `calcAsIs` (passthrough),
   `calcATA` (annual temperature anomaly from MAT and MAT_normal), `calcCMInormal` (mean CMI
   across normals periods), `calcMDC` (monthly drought code from Tmax + PPT, Apr–Sep only — see
   the hard-coded `L_f()` day-length table).
7. Post-processes each resulting raster with `reproducible::postProcessTo()` to match
   `studyArea` + `rasterToMatch`, cached via `reproducible::Cache()`.

Either `tile` **or** `studyArea`+`rasterToMatch` must be supplied, not both. When `studyArea`
is supplied, `whereAmI()` intersects it with `tiles.gpkg` to pick the covering tiles.

### Parallel backends

Two backends are supported, selected via `options(climateData.parallel.backend = ...)` (set in
[R/zzz.R](R/zzz.R), default `"parallel"`):

- `"parallel"` — creates a PSOCK cluster with `parallelly::makeClusterPSOCK()`, bounded by
  `availableCores(constraints = "connections")`.
- `"future"` — uses whatever `future` plan the caller has set.

`buildClimateMosaics*()` dispatch on this option; the worker function
`climateMosaicsParallel()` is intentionally small (just tile id / year / dirs) to keep
serialization cheap. `stopForInvalidBackend()` centralizes the error path.

### SQLite schema

The index at `inst/extdata/ClimateNA_tiles.sqlite` holds four main tables —
`historical`, `historical_normals`, `future`, `future_normals`. `gid` is the Google Drive
file ID; archives are grouped by decade, so many rows share a `gid`. The connection opens in
WAL mode with a 60s busy timeout to allow concurrent writers from the `data-raw/` scripts.

Optional `checksums_<type>` tables are created by `checksums_sql()` when a maintainer runs the
`data-raw/` checksum loops; they may or may not be present in a given copy of the bundled
database. Don't assume they exist when querying.

**The declared schemas in `ClimateNA_sql()` / `checksums_sql()` are not authoritative for
what's actually stored.** `dbCreateTable()` emits e.g. `year CHARACTER` / `created DATETIME`,
but the `data-raw/` scripts populate rows via `dplyr::rows_append(copy = TRUE, in_place = TRUE)`
which can map R column types to its own SQLite types (e.g. R `integer` → `INTEGER`, R `POSIXct`
→ `TIMESTAMP`, R `character` → `TEXT`) — so the PRAGMA-reported column types in the shipped
file can diverge from the R schema strings. When changing a schema, inspect the bundled file
with `PRAGMA table_info(<tbl>)` before writing migration code; it may already be in the
target state.

### Allowed variables

`.allowedClimateVars_{nrm,M,S,Y,MSY}` in [R/available.R](R/available.R) are hard-coded lists
derived by `dir()`-ing a reference ClimateNA output tree (see the comments above each vector).
If ClimateNA adds or renames variables, these lists must be updated — there is no runtime
discovery. `available()` exposes the allowed years, periods, GCMs, and SSPs.

## Dependencies worth knowing

- `reproducible` (≥ 2.0.8) — `preProcess()`, `prepInputs()`, `postProcessTo()`, `Cache()`.
  Many of the long-running behaviors (downloads, reprojection, masking) are delegated here.
- `SpaDES.tools` comes from the `development` branch (see `Remotes:` in DESCRIPTION); the
  r-universe build picks this up automatically, but plain `install.packages()` will not.
- `googledrive` + `httr2` are Suggests but are required at runtime for `getClimateTiles()` and
  `makeClimateDEM()`; functions that need them call `requireNamespace(..., quietly = TRUE)` and
  error out if missing.
