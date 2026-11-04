# ClimateNA data pipeline

A [targets](https://docs.ropensci.org/targets/) pipeline that builds one data release of climateData's tile archives. It runs ClimateNA in the Docker image from `docker/climatena/`, checks every output, cuts the zips, uploads them to Google Drive and builds the tile index.

- `release.yml`: the release (committed): ClimateNA version, years, scenarios, image, Drive folder.
- `_local.R`: per machine (gitignored; copy `_local.R.example`): paths, the container user, the `crew` controller, dry run.
- `_targets.R`, `R/`: the pipeline.

Run from the package root on a compute node:

```r
targets::tar_make(
  script = "data-raw/pipeline/_targets.R",
  store = file.path(local$out_root, "_targets")
)
```

See the maintainer vignette, `vignette("maintaining-climate-data")`, for the full workflow.
