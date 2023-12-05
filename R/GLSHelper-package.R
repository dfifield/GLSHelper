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

  pkg <- installed.packages()
  pkg <- pkg[pkg[,"Package"] == "GeoLight", ]
  if((length(pkg) > 0) && (pkg["Version"] == "2.0.1")) {
      paste0("You have Version 2.0.1 of the GeoLight package installed.") %>%
      paste0(" There is a bug in version 2.0.1 (see https://github.com/slisovski/GeoLight/issues/3)") %>%
      paste0(" that may prevent GeoLight::getElevation() from working properly if your calibration period is close to an equinox.") %>%
      paste0(" You can download the previous version (2.0.0) from https://cran.r-project.org/src/contrib/Archive/GeoLight/") %>%
      paste0(" and install it with devtools::install_local().") %>%
      message()
  }
}
