# ClimateNA data pipeline

A [targets](https://docs.ropensci.org/targets/) pipeline that builds one data release of climateData's tile archives. It runs ClimateNA in the Docker image from `docker/climatena/`, checks every output, cuts the zips, uploads them to Google Drive and builds the tile index.

ClimateNA writes values even where the DEM tile has no elevation, computed at the wrong elevation. When the zips are cut, those cells are set to NODATA unless their centre lies in Canada (GADM 4.1, level 0, holes filled, downloaded once to `<out_root>/boundary/`); the raw outputs keep ClimateNA's values (#40).

- `release.yml`: the release (committed): ClimateNA version, years, scenarios, image, Drive folder.
- `_local.R`: per machine (gitignored; copy `_local.R.example`): paths, the container user, how many workers, dry run, and whether a changed zip may replace its file on Drive (`replace_changed`, off by default). The pipeline uses at most half of each node's cores: a larger worker count is lowered, with a message.
- `_targets.R`, `R/`: the pipeline.

Run from the package root on a compute node:

```r
targets::tar_make(
  script = "data-raw/pipeline/_targets.R",
  store = file.path(local$out_root, "_targets")
)
```

See the maintainer vignette, `vignette("maintaining-climate-data")`, for the full workflow.

## Prototype checks (2026-11-04)

One tile (39) at 3 arcmin, ClimateNA v7.70, `crew_controller_local(workers = 4)`, dry run. A ClimateNA run took about 45 s per year, so a decade batch takes about 8 minutes.

| Check | Result |
|---|---|
| Add a year: historical 2010-2024 to 2010-2025 | ClimateNA ran only 2025. The 2010s batch, zips and uploads were skipped. |
| Add a scenario: future CanESM5 2090-2100, SSP 245 to 245 + 585 | Only the 11 SSP 585 years ran. SSP 245 and historical zips and uploads were skipped. |
| Delete one `.asc` from `Year_2022MSY` | The next run found it and reran only 2022. |
| Set a 5 s run timeout | The batch failed, naming `Year_2023MSY`. Its container was removed and no zips were cut from the truncated output. The report listed the error and the four missing zips. With the timeout restored, only 2023 reran. |
| A compatible new release (v7.70 copied as "v7.71") | Both compatibility checks passed. No ClimateNA runs; no zips or uploads rebuilt. |
| A changed new release (one stored value altered) | `tar_make()` stopped before any historical batch, asking to raise `min_version`. Raising `min_version: historical` to "7.71" regenerated every historical year. |
| A real upload to a scratch release folder | 4 zips (8 MB) in 30 s, MD5s matching. |
| The same, killed 12 s into the uploads, then rerun | The zip uploaded before the kill was found and kept; the other three were uploaded. No duplicate files; MD5s and the index's Drive IDs all match. |
| A second release whose previous index is the first's | All 4 zips reused the first release's Drive files; nothing uploaded. |

The scratch release folders were then moved to Drive's trash.

Three of these checks found defects, fixed on this branch: a damaged output folder was never rerun, a failed batch still had its zips cut, and a failed compatibility check didn't stop the batches.
