#' Read GLS positions
#'
#' @param options [required] List of options at least containing named element
#'  "posnfile" giving the full pathname to the position file to be read. This
#'  file must be either a \code{csv} or Excel (\code{.xls} or \code{.xlsx}) file.
#'
#' @Details The position file must contain (at least) columns named
#'   \code{Date}, \code{Longitude}, and \code{Latitude}.

#' @return A tibble containing columns for \code(Date), \code{Longitude}, and
#'   \code{Latitude} plus any other columns present in the file
#' @export
#'
#' @examples
read_GLS_posns <- function(options) {

  checkmate::qassert(options, "l+")
  checkmate::qassert(options$posnfile, "S+")

  ext <- tools::file_ext(options$posnfile)
  if (ext == "csv") {
    posns <- readr::read_csv(options$posnfile)
  } else if (ext == "xls" || ext == "xlsx") {
    posns <- readxl::read_excel(options$posnfile)
  } else {
    stop(sprintf("GLSSST::read_GLS_posns(): Unrecognized position file type '%s'.", ext))
  }

  mincols <- c("Date", "Latitude", "Longitude")
  # Make sure file has proper columns
  if(!all(mincols %in% colnames(posns))) {
    miscols <- mincols[!(mincols %in% colnames(posns))]
    stop(sprintf("GLSSST::read_GLS_posns(): Missing columns named '%s' in position file '%s'",
    paste0(miscols, collapse = ", "), options$posnfile))
  }

  posns
}
