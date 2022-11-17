#' @title Configuration format

#' @description There are two ways to provide configuration information for
#'   analyses:
#'   \enumerate{
#'       \item as a CSV file whose name is passed as the `cfgfile` argument
#'           to [do_multi_geolocation] to process any number of tag datasets.
#'           This is the easiest and most common approach.
#'       \item as a dataframe passed to the `cfg` argument to [do_geolocation]
#'         which analyses one tag dataset.
#'   }
#'
#'   An example configuration CSV file is included with this package
#'   and can be found at the location given by the following command:
#'
#' `system.file("extdata", "geolocation_settings.csv", package = "GLSHelper")`
#'
#'   Likewise, an example configuration dataframe of the format required for
#'   the \code{cfg} argument to [do_geolocation] is available as `example_cfg`.
#'
#' @details Both the dataframe and CSV file have the same components:
#'
#' @format
#' \describe{
#'   \item{tagName - character}{the name of the tag (e.g., `MK3005 050`). The data for
#'     each tag is stored in unique sub-folder named `tagName` of the `folder`
#'     argument to [do_multi_geolocation].}
#'   \item{include - logical}{whether to process this tag's data. This is a convenient
#'     way to exclude/include certain tags.}
#'   \item{lightFile - character}{the name of the file containing
#'     the light level data. E.g., `MK3005 050_000.lig`,}
#'   \item{colNames - comma separated character string}{names for the columns of the `lightFile`.}
#'   \item{lThresh - integer}{the light threshold level for dawn/dusk. See
#'     \code{\link[GeoLight]{twilightCalc}} in the \pkg{GeoLight} package
#'     for more info.}
#'   \item{maxLightInt - integer}{the duration in minutes over which the tag records the
#'     maximum light interval - typically 10, 5, or 2. This corresponds to the
#'     `maxLight` argument to \code{\link[GeoLight]{twilightCalc}}}
#'    \item{removeFallEqui - logical}{whether to remove positions during the fall
#'      equinox.}
#'    \item{fallEquiStart - date in ISO8601 format ie, YMD HMS}{the start date
#'      of the fall equinox period.}
#'    \item{fallEquiEnd - date in ISO8601 format ie, YMD HMS}{the end date
#'      of the fall equinox period.}
#'    \item{removeSpringEqui - logical}{whether to remove positions during the
#'      spring equinox.}
#'    \item{springEquiStart - date in ISO8601 format ie, YMD HMS}{the start date
#'      of the spring equinox period.}
#'    \item{springEquiEnd - date in ISO8601 format ie, YMD HMS}{the end date
#'      of the spring equinox period.}
#'     \item{doDateFilter - logical}{enable configurable date filter?}
#'    \item{filterStart - date in ISO8601 format ie, YMD HMS}{the start date
#'      of the period to keep.}
#'    \item{filterEnd - date in ISO8601 format ie, YMD HMS}{the end date of the
#'      period to keep.}
#'    \item{deplStart - date in ISO8601 format ie, YMD HMS}{the start date
#'      of the deployment on the animal.}
#'    \item{deplStart - date in ISO8601 format ie, YMD HMS}{the end date
#'      of the deployment on the animal.}
#'    \item{deplLat - numeric}{latitude of the deployment location.}
#'    \item{deplLong - numeric}{longitude of the deployment location.}
#'    \item{calibStart - date in ISO8601 format ie, YMD HMS}{the start date
#'      of the calibration (ie, ground-truthing) period.}
#'    \item{calibEnd - date in ISO8601 format ie, YMD HMS}{the end date
#'      of the calibration (ie, ground-truthing) period.}
#'    \item{calibLat - numeric}{latitude of the calibration location.}
#'    \item{calibLong - numeric}{longitude of the calibration location.}
#'    \item{elev - numeric}{the sun elevation angle when `lThresh` units of light
#'      are recorded. Leave blank (normal case) to have this computed from the
#'      calibration data.}
#'    \item{keepCalibPoints - logical}{retain computed calibration points in position
#'      output? See the [do_multi_geolocation] documentation for help on
#'      differentiaing calibration vs deployment positions in the output.}
#'     \item{calibAsk - logical}{ask the user to confirm each twilight during
#'       the calibration period. This corresponds to the `ask` argument to
#'       \code{\link[GeoLight]{twilightCalc}}.}
#'     \item{deplAsk - logical}{ask the user to confirm each twilight during
#'       the deployment period. This corresponds to the `ask` argument to
#'       \code{\link[GeoLight]{twilightCalc}}.}
#'     \item{createShapefile - logical}{should a shapfile of the points be
#'       created. Shapefiles will be created in the folder indicated by the
#'       `shapefolder` argument to [do_multi_geolocation].}
#'     \item{boxcarSmooth - logical}{should a boxcar (ie, sliding window)
#'       smoothing filter be applied to the deployment positions. If `TRUE`
#'       both the smoothed and unsmoothed positions and paths will be mapped
#'       and returned by [do_multi_geolocation].}
#'     \item{b_iter - integer}{number of iterations of the smoother to execute.
#'       Typical values are 1 or 2.}
# [31] ""          ""                "b_func"
# [34] "b_width"               "b_pad"                 "b_w"
# [37] "b_na.rm"               "b_anchor.ends"         "doDistanceFilter"
# [40] "maxSpeed"              "removeOutliers"        "minX"
# [43] "maxX"                  "minY"                  "maxY"
# [46] "doStatPeriods"         "Xlim"                  "Ylim"
# [49] "createKernel"          "createKernelShapefile" "pcts"
# [52] "projString"            "h"                     "unin"
# [55] "unout"                 "grid"                  "plot_map"
# [58] "readActivity"          "activityType"
#'   \item{country}{Country name}
#'   \item{iso2, iso3}{2 & 3 letter ISO country codes}
#'   \item{year}{Year}
#'   ...
#' }
"example_cfg"
