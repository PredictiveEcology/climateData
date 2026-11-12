#' Candidate sources for land cells ClimateNA leaves empty
#'
#' For each target (a land cell with no ClimateNA values), the nearest land cells that have
#' values, within `maxKm` (geodesic distance), nearest first.
#'
#' @param targets,sources data.frames with `id`, `lon` and `lat` (cell centres, degrees).
#' @param maxKm numeric. Farthest a source may be, in km.
#' @param maxCandidates integer. Most candidates kept per target.
#'
#' @return data.frame with `id` (the target's), `rank` (1 = nearest), `src_lon`, `src_lat` and
#'   `km`; a target with no source within `maxKm` has no rows.
#'
#' @keywords internal
.fillCandidates <- function(targets, sources, maxKm, maxCandidates = 10L) {
  sources <- sources[order(sources$lat), ]
  dLat <- maxKm / 110 # a degree of latitude is at least 110 km
  out <- lapply(seq_len(nrow(targets)), function(i) {
    lon <- targets$lon[i]
    lat <- targets$lat[i]
    lo <- findInterval(lat - dLat, sources$lat) + 1L
    hi <- findInterval(lat + dLat, sources$lat)
    rows <- if (hi >= lo) lo:hi else integer(0)
    dLon <- min(180, dLat / max(cos((abs(lat) + dLat) * pi / 180), 1e-6))
    rows <- rows[abs(sources$lon[rows] - lon) <= dLon]
    if (length(rows) == 0) {
      return(NULL)
    }
    km <- terra::distance(
      cbind(lon, lat),
      cbind(sources$lon[rows], sources$lat[rows]),
      lonlat = TRUE
    )[1, ] /
      1000
    keep <- utils::head(order(km)[km[order(km)] <= maxKm], maxCandidates)
    if (length(keep) == 0) {
      return(NULL)
    }
    data.frame(
      id = targets$id[i],
      rank = seq_along(keep),
      src_lon = sources$lon[rows[keep]],
      src_lat = sources$lat[rows[keep]],
      km = km[keep]
    )
  })
  out <- do.call(rbind, out)
  if (is.null(out)) {
    out <- data.frame(
      id = character(0),
      rank = integer(0),
      src_lon = numeric(0),
      src_lat = numeric(0),
      km = numeric(0)
    )
  }
  out
}

#' Choose each target's source: the nearest candidate ClimateNA gives values at
#'
#' Tries every target's nearest candidate in one `check`, then the next candidate for the
#' targets whose check gave no values, and so on until each has a source or has no candidates
#' left.
#'
#' @param candidates data.frame from [.fillCandidates()].
#' @param check function taking rows of `candidates` (one per target) and returning the `id`s
#'   whose source gave values (a ClimateNA point-mode run, in the pipeline).
#' @param ids character. All targets, including any without candidates.
#'
#' @return data.frame with `id`, `src_lon`, `src_lat` and `km`, one row per id; `NA` source and
#'   `km` for a target left without one.
#'
#' @keywords internal
.chooseSources <- function(candidates, check, ids = unique(candidates$id)) {
  chosen <- data.frame(id = ids, src_lon = NA_real_, src_lat = NA_real_, km = NA_real_)
  pending <- ids
  r <- 1L
  while (length(pending) > 0) {
    try <- candidates[candidates$id %in% pending & candidates$rank == r, , drop = FALSE]
    if (nrow(try) == 0) {
      break
    }
    ok <- try[try$id %in% check(try), , drop = FALSE]
    i <- match(ok$id, chosen$id)
    chosen[i, c("src_lon", "src_lat", "km")] <- ok[c("src_lon", "src_lat", "km")]
    pending <- setdiff(try$id, ok$id)
    r <- r + 1L
  }
  chosen
}
