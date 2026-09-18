#' Latest historical year available as climate tiles
#'
#' The last year for which the bundled tile index lists historical climate data for every
#' requested tile. ClimateNA's annual releases are processed into tiles separately, so this is
#' the latest year that `prepClimateLayers()` can fetch, which can be earlier than the latest
#' year ClimateNA itself has published. It needs no network access.
#'
#' @param tile integer. tile ids (e.g., the output of `tileID()`); `NULL` (the default) means all tiles.
#'
#' @template ClimateNA_msy
#'
#' @return integer
#'
#' @export
#' @importFrom DBI dbDisconnect
#' @importFrom dplyr collect filter
#'
#' @examples
#' latestHistoricalYear()
latestHistoricalYear <- function(tile = NULL, msy = "MSY") {
  dbdf <- ClimateNA_sql(ClimateNA_tiles_sqlite(), "historical")
  on.exit(DBI::dbDisconnect(dbdf[["db"]]), add = TRUE)

  climate_df <- dplyr::filter(dbdf[["df"]], msy %in% !!msy) |>
    dplyr::collect()
  if (!is.null(tile)) {
    climate_df <- climate_df[climate_df$tileid %in% tile, ]
  }
  climate_df <- climate_df[!is.na(climate_df$gid), ]

  tilesPerYear <- tapply(climate_df$tileid, climate_df$year, function(x) length(unique(x)))
  years <- as.integer(names(tilesPerYear))
  max(years[tilesPerYear == length(unique(climate_df$tileid))])
}
