# ClimateNA under Wine

A Docker image for running [ClimateNA](https://climatena.ca/) (a 32-bit Visual Basic 6 Windows program) on Linux, for building climateData's tile archives.

ClimateNA itself is **not** in the image, because its download requires registration. Mount an unpacked ClimateNA directory at `/opt/climatena` at run time.

## Get the image

GitHub Actions builds the image and publishes it as `ghcr.io/predictiveecology/climatena-wine`, with tags `latest` and `sha-<commit>` (see `.github/workflows/docker-climatena.yaml`). To build it locally:

```sh
docker build -t climatena-wine docker/climatena
```

The image contains:
- Debian bookworm, with the base pinned by digest;
- 32-bit Wine 8;
- `xvfb-run`;
- the Visual Basic 6 runtime ClimateNA needs, installed by `winetricks vb6run`, which checks the downloaded file's checksum.

It is about 1.7 GB. Its Wine prefix belongs to uid/gid 1000. For another user, build with `--build-arg UID=... --build-arg GID=...`. Running as a different user stops with exit code 64.

## Run one ClimateNA job

```sh
docker run --rm --user 1000:1000 --cpus 1 --memory 1g \
  -v /path/to/ClimateNA_v7.70:/opt/climatena:ro \
  -v /path/to/dem:/data/dem:ro \
  -v /path/to/output:/data/out \
  ghcr.io/predictiveecology/climatena-wine ClimateNA_v7.70.exe MSY Year_2025.ann /data/dem/can_dem_39.asc /data/out
```

The arguments, all as container paths, are:
1. the executable;
2. the variable set (`M`, `S`, `Y` or `MSY`);
3. the period file (e.g. `Year_2025.ann`, `CanESM5_ssp245@2025.gcm`, `Normal_1991_2020.nrm`, `CanESM5_ssp245_2011-2040.gcm`);
4. the DEM tile;
5. the output directory.

ClimateNA writes `<output>/Year_2025MSY/`. It is single-threaded and small (about 150 MiB at 1 arcmin), so `--cpus 1` is enough. DEM `.asc` files need Windows line endings (`climateData::rewrite_asc()`).

## Validation

ClimateNA v7.60 in this image reproduced the Windows output for tile 39, 2023 (`/MSY`, 3 arcmin DEM). 262 of 265 files are byte-identical. Each of the other 3 (`DD_0_at`, `DD_18_at`, `PPT_at`) differs in one cell, by 1 in the last digit.
