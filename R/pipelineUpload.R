#' What to do with a zip on Drive
#'
#' Given the files of the same name already in the zip's release folder: reuse one with the same
#' MD5 (`"found"`); else replace a differing one in place (`"update"`), so its Drive ID, which
#' the index holds as `gid`, stays the same; else upload a new file (`"upload"`). When several
#' same-named files differ, the one with ID `gid` is updated; if none has it, this stops.
#'
#' @param there data.frame with columns `id` and `md5` (`NA` when Drive gives none).
#' @param md5 character. The zip's MD5.
#' @param gid character or `NULL`. The zip's Drive ID in the index, if it has one.
#'
#' @return list: `action` (`"found"`, `"update"` or `"upload"`) and `id` (`NA` for `"upload"`).
#'
#' @keywords internal
.uploadAction <- function(there, md5, gid = NULL) {
  same <- which(there$md5 %in% md5)
  if (length(same) > 0) {
    return(list(action = "found", id = there$id[same[1]]))
  }
  if (nrow(there) == 0) {
    return(list(action = "upload", id = NA_character_))
  }
  if (!is.null(gid) && any(there$id %in% gid)) {
    return(list(action = "update", id = gid))
  }
  if (nrow(there) == 1) {
    return(list(action = "update", id = there$id))
  }
  stop(
    "several same-named files differ from the zip (",
    paste(there$id, collapse = ", "),
    ") and none is the index's; remove the stray ones"
  )
}
