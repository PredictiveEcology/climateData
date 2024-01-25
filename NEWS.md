# climateData 2.0.0

- improved ClimateNA workflow to use raster tiles instead of individual provinces, and use GDAL VRTs for creating output rasters;
- updated climate data sources generated using ClimateNA v7.4.2 (released Aug 2023). As of ClimateNA v7.41, climate variables with decimals are no longer converted to integers in raster format.

# climateData 1.0.0

## Dependency changes
- drop support for R < 4.2;
- use native R pipe instead of `magrittr`;
- use `terra` instead of `raster`/`sp`;

## Enhancements
- improved speed using `SpatRaster` and `SpatVector` objects throughout

## Bug fixes
- none
