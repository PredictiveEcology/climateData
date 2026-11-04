# ClimateNA under Wine

A Docker image for running [ClimateNA](https://climatena.ca/) (a 32-bit Windows program) on Linux, for the maintainer scripts that build climateData's tile archives.

ClimateNA itself is **not** in the image: its download requires registration. Mount an unpacked ClimateNA directory at `/opt/climatena` at run time.

## Build

```sh
docker build -t climatena-wine docker/climatena
```

The image is Debian bookworm with 32-bit Wine, `xvfb-run`, and the Visual Basic 6 runtime that ClimateNA needs (`winetricks vb6run`). Its Wine prefix belongs to uid/gid 1000; for another user, pass `--build-arg UID=... --build-arg GID=...`.

## Run one ClimateNA job

```sh
docker run --rm --user 1000:1000 --cpus 1 --memory 1g \
  -v /path/to/ClimateNA_v7.70:/opt/climatena:ro \
  -v /path/to/dem:/data/dem:ro \
  -v /path/to/output:/data/out \
  climatena-wine ClimateNA_v7.70.exe MSY Year_2025.ann /data/dem/can_dem_39.asc /data/out/39
```

The arguments are the executable, the variable set (`M`, `S`, `Y` or `MSY`), the period file, the DEM tile, and the output directory, all as container paths. ClimateNA writes `<output>/Year_2025MSY/`. ClimateNA is single-threaded and small (about 150 MiB at 1 arcmin), so `--cpus 1` is enough. DEM `.asc` files need Windows line endings (`climateData::rewrite_asc()`).
