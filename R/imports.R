#' non-exported objects and functions from other packages
#'
#' @importFrom utils getFromNamespace
#' @keywords internal
#' @rdname imports
assessGoogle <- utils::getFromNamespace("assessGoogle", "reproducible")

#' @rdname imports
.listFilesInArchive <- utils::getFromNamespace(".listFilesInArchive", "reproducible")

#' @rdname imports
.whichExtractFn <- utils::getFromNamespace(".whichExtractFn", "reproducible")

#' @rdname imports
getRemoteMetadata <- utils::getFromNamespace("getRemoteMetadata", "reproducible")

#' @rdname imports
googledriveIDtoHumanURL <- utils::getFromNamespace("googledriveIDtoHumanURL", "reproducible")
