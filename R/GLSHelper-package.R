#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom magrittr %>%
## usethis namespace: end

# Print message when user executes "library(GLSHelper). Shamelessly borrowed from mgcv.
.onAttach <- function(...) {
  library(help=GLSHelper)$info[[1]] -> version

  if (!is.null(version)) {
    version <- version[pmatch("Version",version)]
    um <- strsplit(version," ")[[1]]
    version <- um[nchar(um)>0][2]
  } else {
    version <- "Unknown version"
  }

  hello <- paste0("This is GLSHelper ", version, ".")
  packageStartupMessage(hello)
}
