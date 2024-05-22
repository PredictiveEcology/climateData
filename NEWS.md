# climateData 2.2.0

- fix issue extracting tile URLs when tile ids not provided in numerical order, or when numerical order did not match alphabetical order (i.e., `sort(c(2, 11, 12, 3)) != sort(c("2", "3", "11", "12"))`);
- fixed spurious warnings for names in `.dots` passed to `prepClimateLayers()`;
- improved documentation;

# climateData 2.1.0

- add checks for climate variable names and try to catch misspecification of variables by the user in `prepareClimateLayers()`;
- improved documentation;

# climateData 2.0.6

- replace use of 'historic' with 'historical' for all arguments and function names, database tables and entries, and data paths;

# climateData 2.0.4

- fix `calcATA()` calculation

# climateData 2.0.0

## Enhancements

- updated climate data sources generated using ClimateNA v7.4.2 (released Aug 2023). As of ClimateNA v7.41, climate variables with decimals are no longer converted to integers in raster format.
- improved ClimateNA workflow to use raster tiles instead of individual provinces, and use GDAL VRTs for creating output rasters;
- climate variable `CMI` is now produced by ClimateNA directly and does not need to be derived manually;
- improved documentation;
- updated and improved tests;

## Deprecated and Defunct

- removed `makeLandRCS*` functions, as these have been replaced with reworked `calc*` versions (NOTE: `CMI` can now be obtained 'as is' from ClimateNA);

## Dependency changes

- add `fs`, `parallel`, `parallely`, `purrr`, `sf` to Imports;
- move `tools` from Suggests to Imports;
- add `hms`, `SpaDES.tools` to Suggests;

# climateData 1.0.0

## Dependency changes

- drop support for R < 4.2;
- use native R pipe instead of `magrittr`;
- use `terra` instead of `raster`/`sp`;

## Enhancements

- improved speed using `SpatRaster` and `SpatVector` objects throughout

## Bug fixes
- none
