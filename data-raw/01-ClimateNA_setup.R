# load packages -------------------------------------------------------------------------------

## archives and up/downloads
library(archive)
# library(googledrive)

## database
library(DBI)
library(dplyr)
#library(RSQLite)

## parallel processing
library(future)
library(future.apply)
library(future.callr)

## geospatial
# library(geodata)
# library(nngeo)
library(sf)
library(terra)

library(reproducible)
library(rprojroot)

prjDir <- find_root(has_file("climateData.Rproj"))
devtools::load_all(prjDir)

# setup ---------------------------------------------------------------------------------------

## adjust these values for your user / machine

ClimateNAdir <- "C:/Climatena_v742"
ClimateNAexe <- "ClimateNA_v7.42.exe"
ClimateNAdata <- switch(Sys.info()[["sysname"]],
                        Linux = "/mnt/data/climate/ClimateNA_data",
                        Windows = "C:/ClimateNA_data")
stopifnot(dir.exists(ClimateNAdata))

userEmail <- "achubaty@for-cast.ca"
oauthCachePath <- file.path(pkgDir, ".secrets")
googledrive::drive_auth(email = userEmail, cache = oauthCachePath)

## database tracks which data already processed / uploaded
##
##   NOTE: the package version should be the canonical one.
##         although it is useful to work on the shared one (addlDBfile)
##         to allow multiple machines to use/update accordingly,
##         sqlite databases can't always be opened from a network drive.
dbfile <- "ClimateNA_tiles.sqlite"
pkgDBfile <- file.path(prjDir, "inst", "extdata", dbfile)
addlDBfile <- file.path(ClimateNAdata, dbfile)
wrkngDBfile <- file.path(prjDir, dbfile) ## local copy in current directory

if (file.exists(addlDBfile)) {
  file.copy(pkgDBfile, wrkngDBfile) ## always work on a copy
}
