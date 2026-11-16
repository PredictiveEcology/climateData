## Compatibility check of a new ClimateNA release against stored outputs (data-raw/pipeline/).
##
## ClimateNA writes ESRI ASCII grids: 6 header lines, then the cell values as text. The values
## are compared as text-parsed numbers (not via terra), so the check sees exactly what ClimateNA
## wrote. Windows line endings are ignored; any header difference counts as every cell differing.

.readAsc <- function(path) {
  lines <- sub("\r$", "", readLines(path, warn = FALSE))
  list(
    header = gsub("[[:space:]]+", " ", trimws(lines[1:6])),
    values = scan(text = lines[-(1:6)], quiet = TRUE)
  )
}

.compareAsc <- function(a, b) {
  x <- .readAsc(a)
  y <- .readAsc(b)
  if (!identical(x$header, y$header) || length(x$values) != length(y$values)) {
    n <- max(length(x$values), length(y$values))
    return(data.frame(cells = n, differing = as.integer(n), max_abs = Inf))
  }
  d <- abs(x$values - y$values)
  data.frame(cells = length(d), differing = sum(d != 0), max_abs = if (length(d) > 0) max(d) else 0)
}

## One row per .asc file in either folder; a file on only one side has NA counts.
.compareFolders <- function(dirA, dirB) {
  files <- sort(union(list.files(dirA, pattern = "[.]asc$"), list.files(dirB, pattern = "[.]asc$")))
  rows <- lapply(files, function(f) {
    a <- file.path(dirA, f)
    b <- file.path(dirB, f)
    if (!file.exists(a) || !file.exists(b)) {
      return(data.frame(file = f, cells = NA_integer_, differing = NA_integer_, max_abs = NA_real_))
    }
    cbind(file = f, .compareAsc(a, b))
  })
  do.call(rbind, rows)
}

## "compatible" when the file sets match, at most `maxFrac` of all cells differ, and no
## difference exceeds `maxAbs`; otherwise "changed".
.compatVerdict <- function(comparisons, maxFrac = 1e-5, maxAbs = 1) {
  if (nrow(comparisons) == 0 || anyNA(comparisons$cells)) {
    return("changed")
  }
  frac <- sum(comparisons$differing) / sum(comparisons$cells)
  if (frac <= maxFrac && all(comparisons$max_abs <= maxAbs)) "compatible" else "changed"
}
