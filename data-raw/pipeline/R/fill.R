## Filling the land cells ClimateNA leaves empty (cells with a DEM elevation but no values).
##
## ClimateNA's data don't cover some land cells: its future (GCM) data miss some coastal cells,
## and a few cells have no values at all. Such a cell gets ClimateNA's own point-mode output at
## the nearest land cell it covers, run with the empty cell's elevation. Two classes, each with
## one reference period: historical (historical years and normals) and future (future years and
## normals). Point-mode outputs are stored beside the raw period zips, as
## <folder>.fill-<hash>.csv, and written into the zips when they are cut (zip_batch()).

## Which fill class an archive type belongs to.
pipeline_fill_class <- function(type) {
  if (type %in% c("historical", "historical_normals")) "historical" else "future"
}

## Each class's reference period, whose raw outputs tell which land cells are empty: the last
## historical year, and the first GCM's and SSP's first future year. A class whose release has
## no yearly archives uses its normals (the last historical period; the first future one), and
## a class with neither has no reference.
pipeline_fill_reference <- function(cfg) {
  types <- unlist(cfg[["types"]])
  scen <- list(gcm = as.character(cfg[["gcms"]][[1]]), ssp = as.character(cfg[["ssps"]][[1]]))
  ref <- list(
    historical = if ("historical" %in% types) {
      list(
        type = "historical",
        period = as.integer(cfg[["historical_years"]][[2]]),
        msy = "MSY",
        gcm = NA_character_,
        ssp = NA_character_
      )
    } else if ("historical_normals" %in% types) {
      list(
        type = "historical_normals",
        period = as.character(utils::tail(unlist(cfg[["historical_periods"]]), 1)),
        msy = "Y",
        gcm = NA_character_,
        ssp = NA_character_
      )
    },
    future = if ("future" %in% types) {
      c(list(type = "future", period = as.integer(cfg[["future_years"]][[1]]), msy = "MSY"), scen)
    } else if ("future_normals" %in% types) {
      c(
        list(
          type = "future_normals",
          period = as.character(cfg[["future_periods"]][[1]]),
          msy = "Y"
        ),
        scen
      )
    }
  )
  ref[!vapply(ref, is.null, logical(1))]
}

## A ClimateNA point-mode runner for this release: function(points, msy, periodArg) that writes
## `points` (.writePointCsv()) into a fresh folder in the node-local work directory, runs
## ClimateNA there in the image, and returns the path of the CSV it wrote. The caller removes
## that folder.
pipeline_point_runner <- function(image, climatenaExe, climatenaDir, uid, gid, timeout) {
  force(image)
  force(climatenaExe)
  force(climatenaDir)
  force(uid)
  force(gid)
  force(timeout)
  function(points, msy, periodArg) {
    work <- tempfile("fill-", tmpdir = pipeline_work_dir())
    dir.create(work, recursive = TRUE)
    .writePointCsv(file.path(work, "in.csv"), points)
    name <- paste0("cna-", basename(work))
    args <- .climatenaPointDockerArgs(
      name = name,
      image = image,
      climatenaDir = climatenaDir,
      workDir = work,
      exe = climatenaExe,
      msy = msy,
      periodArg = periodArg,
      inCsv = "in.csv",
      outCsv = "out.csv",
      uid = uid,
      gid = gid
    )
    out <- file.path(work, "out.csv")
    ok <- pipeline_run_climatena(args, name, file.path(work, "run.log"), timeout)
    if (!ok || !file.exists(out)) {
      stop(
        "ClimateNA point mode failed for ",
        periodArg,
        " (log: ",
        file.path(work, "run.log"),
        ")"
      )
    }
    out
  }
}

## The land cells to fill and their sources, for every tile and class: data.frame with class,
## tile, cell, lon, lat (the cell's centre), elev (the DEM's text), src_lon, src_lat and km (NA
## when no source was found within `maxKm`). Targets are land cells with no value in the class's
## reference period; candidate sources are land cells with values in it, in any tile, and each
## target gets the nearest candidate whose point run (`run`) gives values. A tile whose
## reference output is missing (its batch failed) is skipped with a warning. `manifests` is a
## dependency only: the reference outputs are read from `outRoot`.
pipeline_fill_points <- function(manifests, tileMasks, demFiles, reference, outRoot, maxKm, run) {
  pipeline_load()
  cols <- c("class", "tile", "cell", "lon", "lat", "elev", "src_lon", "src_lat", "km")
  out <- lapply(names(reference), function(cls) {
    ref <- reference[[cls]]
    folder <- .periodFolder(ref$type, ref$period, ref$msy, ref$gcm, ref$ssp)
    cells <- lapply(names(tileMasks), function(t) {
      spec <- list(type = ref$type, gcm = ref$gcm, ssp = ref$ssp, tile = t)
      zip <- .periodZip(pipeline_tile_dir(outRoot, spec), folder)
      if (!file.exists(zip)) {
        warning("no ", folder, " output for tile ", t, ": its empty land cells are not filled")
        return(NULL)
      }
      m <- tileMasks[[t]]
      if (length(m$land) == 0) {
        return(NULL)
      }
      td <- tempfile("ref-")
      on.exit(unlink(td, recursive = TRUE), add = TRUE)
      utils::unzip(zip, files = file.path(folder, "MAT.asc"), exdir = td)
      a <- .ascRead(file.path(td, folder, "MAT.asc"), m$grid)
      d <- .ascRead(demFiles[[t]], m$grid)
      xy <- terra::xyFromCell(terra::rast(demFiles[[t]]), m$land)
      data.frame(
        tile = as.integer(t),
        cell = m$land,
        lon = xy[, 1],
        lat = xy[, 2],
        elev = substring(d$data, d$start[m$land], d$end[m$land] - 1L),
        empty = substring(a$data, a$start[m$land], a$end[m$land] - 1L) == a$header[["nodata_value"]]
      )
    })
    cells <- do.call(rbind, cells)
    if (is.null(cells) || !any(cells$empty)) {
      return(NULL)
    }
    cells$id <- paste(cells$tile, cells$cell, sep = "_")
    targets <- cells[cells$empty, ]
    candidates <- .fillCandidates(targets, cells[!cells$empty, ], maxKm)
    check <- function(df) {
      t <- targets[match(df$id, targets$id), ]
      pts <- data.frame(
        tile = t$tile,
        cell = t$cell,
        src_lat = df$src_lat,
        src_lon = df$src_lon,
        elev = t$elev
      )
      f <- run(pts, ref$msy, .periodArg(ref$type, ref$period, ref$gcm, ref$ssp))
      on.exit(unlink(dirname(f), recursive = TRUE))
      v <- suppressWarnings(.pointValues(f, "MAT"))
      paste(v$tile, v$cells, sep = "_")
    }
    s <- .chooseSources(candidates, check, ids = targets$id)
    s <- s[match(targets$id, s$id), ]
    data.frame(
      class = cls,
      targets[c("tile", "cell", "lon", "lat", "elev")],
      s[c("src_lon", "src_lat", "km")]
    )
  })
  out <- do.call(rbind, out)
  if (is.null(out)) {
    out <- data.frame(
      class = character(0),
      tile = integer(0),
      cell = integer(0),
      lon = numeric(0),
      lat = numeric(0),
      elev = character(0),
      src_lon = numeric(0),
      src_lat = numeric(0),
      km = numeric(0)
    )
  }
  rownames(out) <- NULL
  out[cols]
}

## One batch's point-mode outputs: for each period, <tileDir>/<folder>.fill-<hash>.csv, with the
## tile's points of the batch's class that have a source. The hash covers those points and the
## period's stamp, so a CSV is redone when either changes, and older CSVs of the period are
## removed. Returns the CSV paths; character(0) for a failed batch (no manifest) or a tile with
## nothing to fill.
fill_batch <- function(spec, manifest, fillPoints, outRoot, run) {
  if (length(manifest) == 0) {
    return(character(0))
  }
  pipeline_load()
  tileDir <- pipeline_tile_dir(outRoot, spec)
  folders <- .periodFolder(spec$type, spec$periods, spec$msy, spec$gcm, spec$ssp)
  pts <- fillPoints[
    fillPoints$class == pipeline_fill_class(spec$type) &
      fillPoints$tile == as.integer(spec$tile) &
      !is.na(fillPoints$src_lon),
    c("tile", "cell", "elev", "src_lon", "src_lat")
  ]
  rownames(pts) <- NULL
  old <- function(folder) {
    f <- list.files(tileDir, full.names = TRUE)
    f[startsWith(basename(f), paste0(folder, ".fill-")) & endsWith(f, ".csv")]
  }
  if (nrow(pts) == 0) {
    unlink(unlist(lapply(folders, old)))
    return(character(0))
  }
  out <- character(length(folders))
  for (i in seq_along(folders)) {
    stamp <- .readStamp(tileDir, folders[i])
    hash <- substr(digest::digest(list(pts, stamp), algo = "xxhash64"), 1, 8)
    f <- file.path(tileDir, paste0(folders[i], ".fill-", hash, ".csv"))
    if (!file.exists(f)) {
      res <- run(pts, spec$msy, .periodArg(spec$type, spec$periods[i], spec$gcm, spec$ssp))
      part <- paste0(f, ".part")
      ok <- file.copy(res, part, overwrite = TRUE) && file.rename(part, f)
      unlink(dirname(res), recursive = TRUE)
      if (!ok) {
        stop("could not store ", basename(f), " in ", tileDir)
      }
    }
    unlink(setdiff(old(folders[i]), f))
    out[i] <- f
  }
  out
}
