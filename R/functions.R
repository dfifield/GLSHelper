#' @export
#' @title Read configuration file
#'
#' @description Read a GLSHelper configuration file. Used
#'    internally and not normally called by the user.
#'
#' @param cfgfile \[character]\cr Required. The full pathname of a configuration
#'    file. If ends in `.xlsx` or `.xls` it is assumed to be an `Excel` file (this is the
#'    preferred format), otherwise it is assumed to be a text file.
#'    This file contains one row per GLS data set to
#'    be processed.
#'
#' @details This function is normally called internally by
#'    `[do_multi_geolocation] to read its configuration info. However, it can
#'    be useful for the end-user during debugging of config file
#'    formatting and reading.
#'
#'    See the \emph{Configuration file format} section of
#'    [do_multi_geolocation] for info on column names, data types, and meanings.
#'
#'   An example configuration  file is included with this package
#'   and can be found at the location given by the following command:
#'
#' `system.file("extdata", "geolocation_settings.xlsx", package = "GLSHelper")`
#'
#'   Note, if you edit a CSV config file with `Excel` bad things may happen to
#'   the formatting of dates and times.
#'
#' @returns A dataframe containing the configuration info.
#' @author Dave Fifield
#'
read_cfg_file <- function(cfgfile){


  if (tools::file_ext(cfgfile) %in% c("xls", "xlsx")){
    if(file.opened(cfgfile))
      stop(sprintf("Could not open configuration file '%s'. Make sure it exists and is not open in Excel!",
                   cfgfile))

    cfgs <- readxl::read_excel(cfgfile,
      col_types = c(
        tagName =  "text",
        include = "logical",
        lightFile	 =  "text",
        log = "logical",
        lThresh	 =  "numeric",
        maxLightInt	 =  "numeric",
        doTwilights = "logical",
        removeFallEqui	 =  "logical",
        fallEquiStart	 =  "date",
        fallEquiEnd	 =  "date",
        removeSpringEqui	 =  "logical",
        springEquiStart	 =  "date",
        springEquiEnd	 =  "date",
        doDateFilter	 =  "logical",
        filterStart = "date",
        filterEnd = "date",
        deplStart	 =  "date",
        deplEnd	 =  "date",
        deplLat	 =  "numeric",
        deplLong	 =  "numeric",
        calibStart	 =  "date",
        calibEnd	 =  "date",
        calibLat	 =  "numeric",
        calibLong	 =  "numeric",
        calibLThresh = "numeric",
        calibYlim = "text",
        elev =  "numeric",
        keepCalibPoints	 =  "logical",
        calibAsk	 =  "logical",
        createShapefile	 =  "logical",
        boxcarSmooth	 =  "logical",
        b_iter = "numeric",
        b_func	 =  "text",
        b_width	 =  "numeric",
        b_pad	 =  "logical",
        b_w	 =  "text",
        b_na.rm	 =  "logical",
        b_anchor.ends	 =  "logical",
        doSpeedFilter	 =  "logical",
        maxSpeed	 =  "numeric",
        removeOutliers	 =  "logical",
        minX	 =  "numeric",
        maxX	 =  "numeric",
        minY	 =  "numeric",
        maxY	 =  "numeric",
        doStatPeriods	 =  "logical",
        statXlim	 =  "text",
        statYlim	 =  "text",
        createKernel	 =  "logical",
        kernelStart = "date",
        kernelEnd = "date",
        createKernelShapefile	 =  "logical",
        pcts	 =  "text",
        projString	 =  "text",
        h	 =  "numeric",
        unin	 =  "text",
        unout =  "text",
        grid =  "numeric",
        plotMap = "logical",
        readActivity = "logical",
        activityType = "text")
    )
  } else { # Text file, assume CSV format
    cfgs <- readr::read_csv(cfgfile,
        col_types = readr::cols( tagName =  "c",
        include = "l",
        lightFile	 =  "c",
        log = "l",
        lThresh	 =  "i",
        maxLightInt	 =  "i",
        doTwilights = "l",
        removeFallEqui	 =  "l",
        fallEquiStart	 =  readr::col_date(),
        fallEquiEnd	 =  readr::col_date(),
        removeSpringEqui	 =  "l",
        springEquiStart	 =  readr::col_date(),
        springEquiEnd	 =  readr::col_date(),
        doDateFilter	 =  "l",
        filterStart = readr::col_date(),
        filterEnd = readr::col_date(),
        deplStart	 =  readr::col_date(),
        deplEnd	 =  readr::col_date(),
        deplLat	 =  "d",
        deplLong	 =  "d",
        calibStart	 =  readr::col_datetime(format = "%Y-%m-%d %H:%M"),
        calibEnd	 =  readr::col_datetime(format = "%Y-%m-%d %H:%M"),
        calibLat	 =  "d",
        calibLong	 =  "d",
        calibLThresh = "d",
        calibYlim = "c",
        elev =  "d",
        keepCalibPoints	 =  "l",
        calibAsk	 =  "l",
        createShapefile	 =  "l",
        boxcarSmooth	 =  "l",
        b_iter = "i",
        b_func	 =  "c",
        b_width	 =  "i",
        b_pad	 =  "l",
        b_w	 =  "c",
        b_na.rm	 =  "l",
        b_anchor.ends	 =  "l",
        doSpeedFilter	 =  "l",
        maxSpeed	 =  "d",
        removeOutliers	 =  "l",
        minX	 =  "d",
        maxX	 =  "d",
        minY	 =  "d",
        maxY	 =  "d",
        doStatPeriods	 =  "l",
        statXlim	 =  "c",
        statYlim	 =  "c",
        createKernel	 =  "l",
        kernelStart = readr::col_date(),
        kernelEnd = readr::col_date(),
        createKernelShapefile	 =  "l",
        pcts	 =  "c",
        projString	 =  "c",
        h	 =  "d",
        unin	 =  "c",
        unout =  "c",
        grid =  "i",
        plotMap = "l",
        readActivity = "l",
        activityType = "c")
      ) %>%
          dplyr::mutate(fallEquiStart = as.POSIXct(fallEquiStart, tz = "UTC"),
             fallEquiEnd = as.POSIXct(fallEquiEnd, tz = "UTC"),
             springEquiStart = as.POSIXct(springEquiStart, tz = "UTC"),
             springEquiEnd = as.POSIXct(springEquiEnd, tz = "UTC"),
             deplStart = as.POSIXct(deplStart, tz = "UTC"),
             deplEnd = as.POSIXct(deplEnd, tz = "UTC"),
             filterStart = as.POSIXct(filterStart, tz = "UTC"),
             filterEnd = as.POSIXct(filterEnd, tz = "UTC"))
  }

  cfgs <- cfgs %>%
      # remove rows that are all NA, Excel likes to insert these.
    dplyr::filter(!dplyr::if_all(dplyr::everything(), is.na)) %>%
      # projstrings that start with a plus sign need to be quoted to protect
      # them from Excel. Remove thos quotes here.
    dplyr::mutate(projString = gsub('"', "", projString))

  cfgs
}

#' @export
#' @title Do geolocation for multiple GLS data sets
#'
#' @description Process multiple GLS logger raw light data sets according to
#'    settings given in a configuration file.
#'
#' @param folder \[character]\cr Required. The name of a folder which contains
#'   the GLS light data for one or more devices. The light data for each device
#'   must be in a separate sub-folder of \code{folder}. The name of each
#'   sub-folder must match the value given in the \code{tagName} column of
#'   `cfgfile`. See \emph{Configuration file format} below.
#'
#' @param cfgfile \[character]\cr Required. The full pathname of a CSV configuration
#'    file. This file contains one line per GLS data set to
#'    be processed. See \emph{Configuration file format} below.
#'
#' @param shapefolder \[character]\cr optional. The full pathname to a folder
#'    where shapefiles will be stored (if requested),
#'
#' @param subset \[character]\cr Optional. A character vector of tag names to
#'    include - this is an easy way to only process a subset of the tags listed
#'    in the configuration file. These names must match the \code{tagName}
#'    column in the configuration file. Tags not listed will be excluded from
#'    analysis, as will listed tags whose `include` column is `FALSE`
#'    in the configuation file.
#'
#' @details This function
#'    reads the config file specified by \code{cfgfile} - each row
#'    specifies the settings for one dataset to process.  It then
#'    processes each data set according to the configuration options given.
#'
#'   \strong{Configuration file format}
#'
#'   The names, data types, and meanings of the columns in the configuration file
#'   file are documented in the list below in the order they appear in the file.
#'
#'   Example configuration CSV and Excel files are included with this package
#'   and can be found in the folder given by the following commands:
#'
#' `system.file("extdata", "geolocation_settings.csv", package = "GLSHelper")`
#'
#'
#' \describe{
#'   \item{tagName - character, required}{The name of the tag (eg., `MK3005 050`). The data for
#'     each tag must be stored in a unique sub-folder of the `folder` argument and
#'     that sub-folder must match the value in the  `tagName` column.}
#'
#'   \item{include - logical, required}{Whether to process this tag's data. This is a convenient
#'     way to exclude/include certain tags.}
#'
#'   \item{lightFile - character, required}{The name of the file containing
#'     the light level data. eg., `"MK3005 050_000.lig"`.}
#'
#'   \item{log - logical, required}{Whether the raw light values should be log transformed.
#'     This is common for devices that record the entire range of light intensities
#'     such as the Migrate Technology Integeo series tags.}
#'
#'   \item{lThresh - integer, required}{The light threshold level for dawn/dusk. See
#'     \code{\link[GeoLight]{twilightCalc}} in the \pkg{GeoLight} package
#'     for more info.}
#'
#'   \item{maxLightInt - integer, required}{The duration in minutes over which the tag records the
#'     maximum light interval - typically 10, 5, or 2. This corresponds to the
#'     `maxLight` argument to \code{\link[GeoLight]{twilightCalc}}}
#'
#'   \item{doTwilights - logical, required}{Whether to do the twilight annotation process. If \code{TRUE}
#'      \code{\link[TwGeos]{preprocessLight}} is invoked to interactively annotate
#'      twilights (see \url{https://geolocationmanual.vogelwarte.ch/twilight.html}).
#'      Once twilights have been annotated, the are save in \code{tagName twilights.rds} in the
#'      the tag's data folder for future use. If \code{FALSE}, the twilights are read from
#'      the previously saved file.}
#'
#'   \item{removeFallEqui - logical, required}{Whether to remove positions during the fall
#'      equinox.}
#'
#'   \item{fallEquiStart, fallEquiEnd - dates in ISO8601 format (YMD, e.g. 2013-09-07), required}{Start/end
#'    dates of the fall equinox period.}
#'
#'   \item{removeSpringEqui - logical, required}{Whether to remove positions during the
#'      spring equinox.}
#'
#'   \item{springEquiStart, springEquiEnd - dates in ISO8601 format (YMD, e.g. 2013-09-07), required}{Start/end
#'    dates of the spring equinox period.}
#'
#'   \item{doDateFilter - logical, required}{Enable configurable date filter? Only positions
#'     with dates between `filterStart` and `filterEnd` will be retained in the output.}
#'
#'   \item{filterStart, filterEnd - date in ISO8601 format (YMD, e.g. 2013-09-07),
#'      required if `doDateFilter` is \code{TRUE}}{Start/end dates of the period to keep.}
#'
#'   \item{deplStart, deplEnd - date in ISO8601 format (YMD, e.g. 2013-07-13), optional}{Start date
#'      of the deployment on the animal. If provided, locations are only be estimated between
#'      \code{deplStart} and \code{deplEnd}.}
#'
#'   \item{deplLat, deplLong - numeric, optional}{Coordinates of the deployment location.
#'    All geographic coordinates are assumed to reference the WGS84 datum.}
#'
#'   \item{calibStart, calibEnd - dates in ISO8601 format (YMD HM, e.g. 2013-06-28 15:00), required}{
#'      Start/end date/time of the calibration (ie, ground-truthing) period.}
#'
#'   \item{calibLat, calibLong - numeric, required}{Coordinates of the calibration location. All geographic
#'     coordinates are assumed to reference the WGS84 datum.}
#'
#'   \item{elev - numeric, optional}{The sun elevation angle when `lThresh` units of light
#'      are recorded. Leave blank (normal case) to have this computed from the
#'      calibration data.}
#'
#'   \item{keepCalibPoints - logical, required}{Retain computed calibration points in position
#'      output? See the `Value` section below for help on
#'      distinguishing calibration vs deployment positions in the output.}
#'
#'   \item{calibAsk - logical, required}{Ask the user to confirm each twilight during
#'       the calibration period? This corresponds to the `ask` argument to
#'       \code{\link[GeoLight]{twilightCalc}}.}
#'
#'   \item{createShapefile - logical, required}{Should a shapefile of the points be
#'       created. Shapefiles will be created in the folder indicated by the
#'       `shapefolder` argument. The values of several parameters are incorporated
#'       into the name of the resulting shapefile. For example, if `tagName = "MK3005 050"`,
#'      `lThresh = 16`, `elev = -3.48`, `boxcarSmooth = TRUE`, and `b_iter = 2`
#'      the shapefiles will be named:
#'       \cr
#'      `MK3005 050_thr_16_elev_-3.48_smooth2`
#'       }
#'
#'   \item{boxcarSmooth - logical, required}{Should a boxcar (ie, sliding window)
#'       smoothing filter be applied to the deployment positions? The smoother
#'       adjusts the coordinates of each point taking into account the current
#'       point's coordinates and those of some number of previous and succeeding
#'       points. The number of points to consider, their weightings, and the
#'       function used to combine them are specified by the \code{b_width},
#'       \code{b_w}, and \code{b_func} options respectively. For example, with
#'       \code{b_width = 5}, \code{b_w = c(1,2,3,2,1)}, and \code{b_func =
#'       'weighted_mean'} the current position's latitude/longitude is the weighted
#'       mean (in a 1:2:3:2:1 ratio) of the second previous, previous, current,
#'       subsequent, and second subsequent position’s latitude/longitude.
#'       If `boxcarSmooth` is `TRUE`, then both the smoothed and unsmoothed
#'       positions and paths will be mapped and returned by [do_multi_geolocation].}
#'
#'   \item{b_iter - integer, required if `boxcarSmooth` is \code{TRUE}}{
#'       Boxcar smoothers are often applied iteratively and this value indicates
#'       the number of iterations of the smoother to execute. Typical values are
#'       1 or 2.}
#'
#'   \item{b_func - character, required if `boxcarSmooth` is \code{TRUE}}{
#'       The name of the function to apply to the
#'       coordinates in the sliding window. Typically this is 'weighted.mean'
#'       and it is unlikely you will need to change this. However any function
#'       that takes a vector of numbers as its 1st argument, a series of weights,
#'       `w`, and a logical `na.rm`, and returns a single numeric value will
#'        work.}
#'
#'   \item{b_width - integer, required if `boxcarSmooth` is \code{TRUE}}{
#'       The width of the sliding window (or boxcar). This controls
#'       how many preceding and succeeding points influence the coordinates of
#'       the current point. Normally this is an odd number (a warning is printed
#'       if not) so that the same number of preceding and succeeding points
#'       exert influence.}
#'
#'   \item{b_pad - logical, required if `boxcarSmooth` is \code{TRUE}}{
#'     Should the first (last) positions in the track
#'     be padded with
#'     enough extra copies of themselves to provide a complete `b_width` wide
#'     window when processing the first and last windows, respectively.
#'     For example, without padding, the first full window
#'     (e.g., with `b_width = 5`) will be centered on the third position, and likewise
#'     the last full window will be centered on the third-last position. This
#'     will cause the resulting smoothed track to have 4 less points than the
#'     original (two lost at the beginning and two at the end).
#'     This is because each full window-width of points in the original
#'     track is converted to a single point in the output (typically as the
#'     weighted mean of its constituents). When `b_pad` is `TRUE`, a sufficient
#'     number of copies of the first (last) coordinate are inserted at the
#'     beginning (end) of the track so that the first (last) full window
#'     is centered on the first (last) track position. In this case,  the number
#'     of points in the smoothed track is the same as that in the original.}
#'
#'   \item{b_w - integer, required if `boxcarSmooth` is \code{TRUE}}{
#'    A vector of length `b_width` giving the relative weights
#'    of each position in the sliding window.}
#'
#'   \item{b_na.rm - logical, required if `boxcarSmooth` is \code{TRUE}}{
#'     Passed directly to `b_func` (typically,
#'     `weighted.mean`) indicating whether `NA` values should be stripped from
#'     each window before applying `b_func`.`NA` values can occur in coordinates
#'     if position estimation fails for some points.}
#'
#'   \item{b_anchor.ends - logical, required if `boxcarSmooth` is \code{TRUE}}{
#'     If `TRUE` fhe first and last positions in
#'     the track are not modified by the smoother. This is useful, for example,
#'     when the first and
#'     final positions of a track are known to be at a specific location
#'     (e.g., breeding colony).}
#'
#'   \item{doSpeedFilter - logical, required}{Should a maximum-speed
#'     filter be applied. If `TRUE`, successive positions requiring, on average,
#'      speeds of more than `maxSpeed` km/hour will be considered unrealistic and
#'     be removed. Uses \code{\link[GeoLight]{distanceFilter}} from \pkg{GeoLight}.}
#'
#'   \item{maxSpeed - numeric, required if `doSpeedFilter` is \code{TRUE}}{
#'     The maximum realistic animal speed in km/h.}
#'
#'   \item{removeOutliers - logical, required}{Should positional outliers outside a
#'     bounding box given by `minX, maxX, minY, maxY` be removed?}
#'
#'   \item{minX, maxX, minY, maxY - numeric, required if `removeOutliers` is \code{TRUE}}{
#'      The min/max longitude and latitudes
#'      respectively of the bounding box for removing outliers. Positions whose
#'      coordinates fall on the bounding box are removed.}
#'
#'   \item{doStatPeriods - logical, required}{Should stationary periods be computed? See
#'     \code{\link[GeoLight]{changeLight}} for more information.}
#'
#'   \item{statXlim, statYlim - numeric, optional}{The longitude and latitude limits for the site map
#'      resulting from calculating stationary periods. See `doStatPeriods` and
#'      \code{\link[GeoLight]{siteMap}}.}
#'
#'   \item{createKernel - logical, required}{Should kernel utilization distributions be
#'     computed? if `TRUE`, \code{\link[adehabitatHR]{kernelUD}} is used to
#'     compute the kernel surface and \code{\link[adehabitatHR]{getverticesHR}}
#'     is use to produce the requested percentage contours. See also
#'     `pcts, projString, h, grid, unin,` and `unout` below.}
#'
#'   \item{createKernelShapefile - logical, required if `createKernel` is \code{TRUE}}{
#'      Should shapefile(s) of the contoured
#'      kernel utilization distributions be created. If `TRUE`, one shapefile
#'      will be created for each value of `pcts`. If `boxcarSmooth = TRUE`,
#'      the smoothed positions will be used for the kernel otherwise the original
#'      unsmoothed points will be. The values of several parameters are incorporated
#'      into the name of the resulting shapefile. For example,
#'      if `tagName = "MK3005 050"`,
#'      `lThresh = 16`, `elev = -3.48`, `boxcarSmooth = TRUE`, `b_iter = 2` and
#'      `pcts = c(50, 95)`, then two shapefiles will be produced with names:
#'       \cr
#'      `MK3005 050_thr_16_elev_-3.48_smooth2_UD_50`, and
#'       \cr
#'      `MK3005 050_thr_16_elev_-3.48_smooth2_UD_95`
#'      }
#'
#'   \item{pcts - integer, required if `createKernel` is \code{TRUE}}{Vector of
#'     UD contour percentages to compute (e.g., c(50, 95)).}
#'
#'   \item{projString - character, required if `createKernel` is \code{TRUE}}{
#'     Because the kernel surface to be contoured
#'     must be computed based on Cartesian
#'     coordinates, the un-projected geographic coordinates (i.e.
#'     latitude/longitude) must be projected to a 2D coordinate system first.
#'     This parameter gives the PROJ4 string of the coordinate system to use
#'     (e.g., `"+proj=merc +datum=WGS84 +units=m +ellps=WGS84"`). If
#'     left blank, then a Lambert conformal conic projection will be chosen with
#'     central meridian at the `mean` of the point's longitudes, and standard
#'     parallels at 1/6 and 5/6 of the latitudinal range of the positions.}

#'   \item{h - integer, required if `createKernel` is \code{TRUE}}{
#'     The kernel smoothing bandwidth parameter - this is the
#'     `h` parameter passed to \code{\link[adehabitatHR]{kernelUD}}. If left
#'     blank, then the `href` method is used. Otherwise, this value is
#'     in the same units as the chosen projection, typically meters
#'     (see `projString`)}
#'
#'   \item{unin, unout - character, required if `createKernel` is \code{TRUE}}{
#'     These are the input and output units respectively
#'      for the call to
#'     \code{\link[adehabitatHR]{getverticesHR}} to create the UD contours.
#'     `unin` must the same as units of the chosen projection (typically `m` for
#'     "meters", see `projString`). `unout` is typically set to `km2` so that
#'     the area of the resulting UD contour is in kilometres squared.}
#'
#'   \item{grid - integer, required if `createKernel` is \code{TRUE}}{
#'   The number of grid cells for the computed kernel surface. This
#'   is passed as the `grid` argument to \code{\link[adehabitatHR]{kernelUD}}.
#'   A typical value is 500.}
#'
#'   \item{plotMap - logical, required}{Should a Leaflet map be plotted for each tag data
#'    set? Maps can only be plotted in this way when [do_multi_geolocation]
#'    is called from an interactive R script. This argument will be ignored when
#'    `do_multi_geolocation` is called from a knitted RMD document. To plot maps
#'    in a knitted RMD, capture the object
#'    returned by `do_multi_geolocation` and print the maps with auxilliary code
#'    inserted directly into a markdown chunk. See `Examples`.}
#'
#'   \item{readActivity - logical, required}{Should the tag activity data be read?}
#'
#'   \item{activityType - character, required if `readActivity` is \code{TRUE}}{
#'     Type of activity data file to expect.
#'     Currently only accepts `coarse` or `fine` corresponding to the
#'     coarse-scale or fine-scale activity data of BAS/BioTrack devices.}
#'}
#' @return A list with one element for each tag data set processed. The names
#' of the list elements are taken from the `tagName` column of the input
#' configuration file. Each element of this list is itself a list containing:
#' \itemize{
#'  \item{posns - \[dataframe] the computed positions, with columns:}
#'  \itemize{
#'      \item{\code{tFirst, tSecond, type} - sun rise/set times. See
#'      \code{\link[GeoLight]{twilightCalc} for info.}}
#'      \item{\code{src} - the source of the position, either \code{"Calib"} for
#'         calibration period or \code{"Deployment"} for deployment period.}
#'      \item{\code{lng, lat} - the longitude and latitude.}
#'      \item{\code{smthlng, smthlat} - the smoothed longitude and latitude, if
#'          requested.}
#'  }
#'  \item{\code{elev} - \[numeric] the sun elevation angle used in the
#'      calculation of positions.}
#'  \item{\code{light} - \[dataframe] the raw light data after applying any
#'   optional date filtering.}
#'  \item{\code{act} - \[dataframe] the activity data.}
#'  \item{\code{calib} - \[dataframe] the raw light data during calibration.}
#'  \item{\code{cfg} - \[dataframe] the config settings used.}
#'  \item{\code{m} - a Leaflet map.}
#' }
#'
#' @section Author: Dave Fifield
#'
#' @examples
#' res <- do_multi_geolocation(folder = here::here("Data/Tag data"),
#'                             cfgfile = here::here("Data/geolocation_settings.csv"),
#'                             shapefolder = here::here("GIS/Shapefiles"))
#'
#' # To extract and display maps in a knitted RMD document, place the
#' # following code in a code chunk:
#'
#' \code{# Display maps- note this code is safe to execute both interactively and
#' # during knitting. tagList wraps the Leaflet maps in the appropriate
#' # HTML to allow them to appear in a knitted document. This will produce
#' # a series of maps in the output, each labeled with the name of the tag name.
#'
#' if (isTRUE(getOption('knitr.in.progress'))) {
#'   html <- list()
#'   for (i in 1:length(res)) {
#'     html <- list(html,
#'                  h2(paste0(i, "_", names(res)[i])),
#'                  res[[i]]$map)
#'   }
#'
#'   tagList(html)
#' }}
#'
do_multi_geolocation <- function(folder, cfgfile, shapefolder = NULL,
                               subset = NULL) {
  # Display warnings as soon as they happen
  opt <- getOption("warn")
  options(warn = 1)

  cfgs <- read_cfg_file(cfgfile) %>%
    dplyr::filter(include == TRUE)

  if (!is.null(subset))
    cfgs %<>% dplyr::filter(tagName %in% subset)

  res <- cfgs %>%
    split(1:nrow(.)) %>%
    purrr::map(do_geolocation, folder = folder, shapefolder = shapefolder) %>%
    purrr::set_names(cfgs$tagName)

  options(warn = opt)
  res
}

#' @noRd
#' @title Do geolocation for one GLS data set
#'
#' @description Process a single GLS logger raw light data set according to
#'    settings given in \code{cfg} using the \pkg{GeoLight} package.
#'
#' @param cfg \[character]\cr Required. A dataframe of configuration information
#'    to control the processing.
#'
#' @param folder \[character]\cr Required. The path to the folder that contains
#'    the tag data stored in separate sub-folders for each tag. The name of
#'    each sub-folder must match the \code{cfg$tagName}.
#'
#' @param shapefolder \[character]\cr optional. The full pathname to a folder
#'    where shapefiles will be stored (if requested).
#'
#' @details The details
#'
#'    There are quite strict naming conventions for subfolders, light files, etc.
#'
#' @return A list containing:
#' \itemize{
#'  \item{posns - \[dataframe] the computed positions, with columns:}
#'  \itemize{
#'      \item{\code{tFirst, tSecond, type} - see
#'      \code{\link[GeoLight]{twilightCalc} for info.}}
#'      \item{\code{src} - the source of the position, either \code{"Calib"} for
#'         calibration period or \code{"Deployment"} for deployment period.}
#'      \item{\code{lng, lat} - the longitude and latitude.}
#'      \item{\code{smthlng, smthlat} - the smoothed longitude and latitude, if
#'          requested.}
#'  }
#'  \item{\code{elev} - \[numeric] the sun elevation angle used in the
#'      calculation of positions.}
#'  \item{\code{light} - \[dataframe] the raw light data after applying optional date
#'     filtering}
#'  \item{\code{act} - \[dataframe] the activity data.}
#'  \item{\code{calib} - \[dataframe] the raw light data during calibration.}
#'  \item{\code{cfg} - \[dataframe] the config settings used.}
#'  \item{\code{m} - a Leaflet map.}
#' }
#' @section Author: Dave Fifield
do_geolocation <- function(cfg, folder, shapefolder = NULL) {
  message(sprintf("\n\nProcessing tag %s", cfg$tagName))

  tagDir <- file.path(folder, cfg$tagName)
  calibFile <- file.path(tagDir, paste0(cfg$tagName," calibration twilights.rds"))
  twiFile <- file.path(tagDir, paste0(cfg$tagName, " twilights.rds"))
  calibLoc <- c(cfg$calibLong, cfg$calibLat)
  w <- as.numeric(unlist(strsplit(cfg$b_w, split = ",")))
  statXlim <- as.numeric(unlist(strsplit(cfg$statXlim, split = ",")))
  statYlim <- as.numeric(unlist(strsplit(cfg$statYlim, split = ",")))
  calibYlim <- as.numeric(unlist(strsplit(cfg$calibYlim, split = ",")))
  pcts <- as.numeric(unlist(strsplit(cfg$pcts, split = ",")))

  # read light data, implicitly assumes first row is headers
  # (it is some stuff for BAS loggers which gets ignored)
  message("Reading light data")
  lightfile <- file.path(tagDir, cfg$lightFile)
  ext <- tools::file_ext(lightfile)
  if (ext == "lux") {
    alldat <- GeoLight::luxTrans(lightfile)
  } else if (ext == "lig")
    alldat <- GeoLight::ligTrans(lightfile)
  else if (ext == "gle"){
    alldat <- GeoLight::gleTrans(lightfile)
  }  else if (ext == "glf"){
    alldat <- GeoLight::glfTrans(lightfile)
  } else {
    stop(sprintf("GeoLight::do_geolocation(): Unrecognized file extension '%s'."))
  }

  # do we want to log the light values
  if (cfg$log) {
    alldat <- mutate(alldat, light = log(light+0.0001) + abs(min(log(light+0.0001))))
  }

  # read activity data if it exists
  if (cfg$readActivity) {
    actfile <- file.path(tagDir, sub("lig$", "act", cfg$lightFile))
    if (file.exists(actfile)) {
      message("Reading activity data")
      if (cfg$activityType == "coarse") {
        act <- suppressWarnings(readr::read_csv(actfile, skip = 1, col_names = FALSE,
              col_types = c("ccdi"))) %>%
          purrr::set_names(c("status", "datetime", "datesec", "act")) %>%
          dplyr::filter(status == "ok") %>%
          dplyr::mutate(datetime = lubridate::dmy_hms(datetime))
      } else { # fine scale activity data
        act <- suppressWarnings(readr::read_csv(actfile, skip = 1, col_names = FALSE,
              col_types = c("ccdic"))) %>%
          purrr::set_names(c("status", "datetime", "datesec", "seconds", "act")) %>%
          dplyr::filter(status == "ok") %>%
          dplyr::mutate(datetime = lubridate::dmy_hms(datetime))
      }
    } else {
      message("No activity data file found, skipping.")
      act <- NULL
    }
  }

  # calibration
  calib <- dplyr::filter(alldat, datetime >= as.POSIXct(cfg$calibStart, tz = "UTC")
                         & datetime <= as.POSIXct(cfg$calibEnd, tz = "UTC"))

  if(length(calibYlim) > 1 &&  !any(is.na(calibYlim))) {
    plot(
      calib$datetime,
      calib$light,
      type = "l",
      main = paste0(cfg$tagName, " calibration"),
      ylim = calibYlim
    )
  } else {
    plot(
      calib$datetime,
      calib$light,
      type = "l",
      main = paste0(cfg$tagName, " calibration")
    )
  }

  message("Getting calibration period twilights and positions")
  calibtwi <- GeoLight::twilightCalc(datetime = calib$datetime,
                                     light = calib$light,
                                     LightThreshold = cfg$calibLThresh,
                                     maxLight = cfg$maxLightInt,
                                     ask = cfg$calibAsk) %>%
    dplyr::mutate(src = "Calib")

  # If no default elev given then compute from calibration data
  elev.long <- NA
  if (is.na(cfg$elev)) {
    message("Getting sun elevation angle")
    elev <- getElevation(twl = calibtwi,
                       known.coord = calibLoc,
                       method = "gamma")

    # New version of getElevation in GeoLight >= 2.0.1 returns a vector with the
    # elevation angle in second element. Need to check length of elev to
    # figure out what to do.
    if (length(elev) > 1){
      elev.long <- elev
      elev <- 90-elev[1]
    }

    message(sprintf("Elevation angle calculated from calibration period: %.2f", elev))
  } else {
    elev = cfg$elev
    message(sprintf("Elevation angle set from config file: %.2f", elev))
  }

  # keep calibration points?
  if (cfg$keepCalibPoints) {
    calibCoord <- GeoLight::coord(calibtwi$tFirst, calibtwi$tSecond,
                                  calibtwi$type, degElevation = elev)
    cat("\n")
    calibPoints <- cbind(calibtwi, calibCoord) %>%
      purrr::set_names(c(names(calibtwi), c("lng", "lat")))
  }

  dat <- alldat

  # filter non-deployment dates
  if (!is.na(cfg$deplStart) && !is.na(cfg$deplEnd)) {
    dat <- dplyr::filter(dat, datetime >= cfg$deplStart & datetime <=  cfg$deplEnd)
  } else
    warning("One of deplStart or deplEnd is blank in the configuration file." %>%
              paste0(" Will not be able to remove non-deployment periods."))

  # arbitrary date filter useful to exclude at colony time, or delineate wintering area, etc
  if (cfg$doDateFilter) {
    dat <- dplyr::filter(dat, datetime >= cfg$filterStart &
                           datetime <=  cfg$filterEnd)
  }


  # Get twilights
  if (!cfg$doTwilights) {
    if (file.exists(twiFile)) {
      message("Loading existing twilights from %s", twFile)
      twi <- readRDS(twiFile)
    } else {
      stop(sprintf("Could not find previously saved twilight file '%s'", twiFile))
    }
  } else {
    message("Loading twilights from ", twiFile)

    tagdata <- dat %>%
      dplyr::select(datetime, light) %>%
      dplyr::rename(Date = datetime,
                    Light = light)

    # get twilights and convert to GeoLight format
    twi <- TwGeos::preprocessLight(
      tagdata,
      threshold = cfg$lThresh,
      lmax = cfg$lThresh * 1.1,
      # make sure threshold is in the plot
      offset = 12
    ) %>%
      dplyr::filter(!Deleted) %>%
      TwGeos::export2GeoLight() %>%
      dplyr::mutate(src = "Deployment") # mark these as deployment period
    saveRDS(twi, twiFile)
  }
  # twi <- GeoLight::twilightCalc(datetime = dat$datetime,
  #                               light = dat$light,
  #                               LightThreshold = cfg$lThresh,
  #                               maxLight = cfg$maxLightInt,
  #                               ask = cfg$deplAsk) %>%
  # dplyr::mutate(src = "Deployment") # mark these as deployment period


  # remove spring Equinox
  if (cfg$removeSpringEqui) {
    twi <- dplyr::filter(twi, tFirst <= cfg$springEquiStart |
                           tSecond >= cfg$springEquiEnd)
  }

  # remove fall Equinox
  if (cfg$removeFallEqui) {
    twi <- dplyr::filter(twi, tFirst <= cfg$fallEquiStart |
                           tSecond >= cfg$fallEquiEnd)
  }

  # calculate locations
  coord <- GeoLight::coord(twi$tFirst, twi$tSecond, twi$type, degElevation = elev)
  cat("\n")

  # remove points outside specified area?
  if (cfg$removeOutliers) {
    message("Removing spatial outliers")
    # Remove positions way outside the study area for example on the other side of the world
    cond <- (coord[, 1] >= cfg$minX & coord[, 1] <= cfg$maxX) &
            (coord[, 2] >= cfg$minY & coord[, 2] <= cfg$maxY)
    coord <- coord[cond, ]
    twi <- twi[cond, ]
  }

  # remove unrealistic positions - not sure what distance should be
  if (cfg$doSpeedFilter) {
    message("Doing speed filtering")
    filt <- NA
    filt <- GeoLight::distanceFilter(twi$tFirst, twi$tSecond, twi$type,
                    degElevation = elev, distance = cfg$maxSpeed, units = "hour")

    #Need to check this since sometimes the distance filter fails
    if (length(filt) == 1 && is.na(filt)) {
      warning("Speed filter failed.", immediate. = TRUE)
    } else {
      coord <- coord[filt,]
      twi <- twi[filt,]
    }
  }

  # get stationary periods?
  if (cfg$doStatPeriods) {
    message("Doing residency analysis.")
    site <- GeoLight::changeLight(twi$tFirst, twi$tSecond, twi$type,
                              rise.prob = 0.1, set.prob = 0.1, days = 5)$site
    if (!is.na(statXlim) && !is.na(statYlim))
      GeoLight::siteMap(coord, site, xlim = statXlim, ylim = statYlim)
    else
      GeoLight::siteMap(coord, site)
  }

  # create trajectory
  traj <- cbind(twi, coord) %>%
    purrr::set_names(c(names(twi), c("lng", "lat"))) %>%
    dplyr::filter(!is.na(lat))

  # smoothing - iteratively smooth cfg$b_iter times
  if (cfg$boxcarSmooth) {
    message("Creating smoothed track")
    smth <- cbind(traj$lng, traj$lat)
    for (i in cfg$b_iter) {
      smth[, 1] <- boxcar(smth[, 1], bfunc =  cfg$b_func, width = cfg$b_width,
                          pad = cfg$b_pad, w = w, na.rm = cfg$b_na.rm,
                          anchor.ends = cfg$b_anchor.ends)
      smth[, 2] <- boxcar(smth[, 2], bfunc =  cfg$b_func, width = cfg$b_width,
                          pad = cfg$b_pad, w = w, na.rm = cfg$b_na.rm,
                          anchor.ends = cfg$b_anchor.ends)
    }
    traj <- cbind(traj, smth) %>%
      purrr::set_names(c(names(traj), c("smthlng", "smthlat")))

    # combine calibration data in with deployment data?
    if (cfg$keepCalibPoints) {
      traj <- calibPoints %>%
        dplyr::mutate(smthlng = lng,
               smthlat = lat) %>%
        rbind(traj)
    }

    lngcol <- "smthlng"
    latcol <- "smthlat"
  } else {
    # combine calibration data in with deployment data?
    if (cfg$keepCalibPoints) {
      traj <- rbind(traj, calibPoints)
    }
    lngcol <- "lng"
    latcol <- "lat"
  }

  # Create shapefile
  if (cfg$createShapefile)
    do_shapefile(traj, lngcol = lngcol, latcol = latcol, elev = elev, cfg = cfg,
                 shapefolder = shapefolder)

  # create seasons for mapping before/after equinoxes and also add month
  fallequi <- lubridate::ymd(paste(lubridate::year(cfg$fallEquiStart), 9, 21,
                                   sep = "-"), tz = "GMT")
  sprequi <- lubridate::ymd(paste(lubridate::year(cfg$springEquiStart), 3, 21,
                                  sep = "-"), tz = "GMT")

  # Augment traj with month and season
  traj <- traj %>%
    dplyr::mutate(month = factor(format(tFirst, "%b"),
                          levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
    dplyr::mutate(season = dplyr::case_when(
      tFirst <= fallequi | tFirst >= sprequi ~ "spring_summer",
      tFirst > fallequi & tFirst < sprequi ~ "fall_winter",
      TRUE ~ NA_character_))

  # Create palettes
  pal.season <- leaflet::colorFactor(c(spring_summer = "brown",
                                       fall_winter = "blue"),
                                     domain = traj$season)
  pal <- leaflet::colorFactor(
               c(Jan = "cyan", Feb = "magenta", Mar = "grey60", Apr = "black",
                 May = "sienna", Jun = "red", Jul = "olivedrab1",
                 Aug = "white", Sep = "plum1", Oct = "forestgreen",
                 Nov = "yellow", Dec = "blue"), domain = traj$month)


  # Create map
  groups <- c("points", "path", "calib loc", "deploy loc", "season")
  m <- traj %>%
    dplyr::filter(src == "Deployment") %>%
    leaflet::leaflet( width = "100%") %>%
    leaflet::addTiles() %>%
    leaflet::addMarkers(lng = cfg$calibLong, lat = cfg$calibLat,
                        label = "Calibration location", group = "calib loc")

  # Add deployment marker
  if (!is.na(cfg$deplLong) && !is.na(cfg$deplLat)) {
    m <- leaflet::addMarkers(
      m,
      lng = cfg$deplLong,
      lat = cfg$deplLat,
      label = "Deployment location",
      group = "deploy loc"
    )
  }

  # Continue
  m <- m %>%
    # leaflet::setView(mean(na.omit(traj$lng)), mean(na.omit(traj$lat)), zoom = 5) %>%
    leaflet::fitBounds(
      lng1 = min(traj$lng),
      lat1 = min(traj$lat),
      lng2 = max(traj$lng),
      lat2 = max(traj$lat)
    ) %>%
    # Monthly colored points
    leaflet::addCircleMarkers(lng = ~lng, lat = ~lat, radius = 3,
                              color = ~pal(month), label = ~tFirst,
                              stroke = FALSE, fillOpacity = 0.75,
                              group = "points") %>%
    # Seasonally colored points
    leaflet::addCircleMarkers(lng = ~lng, lat = ~lat, radius = 3,
                              color = ~pal.season(season), label = ~tFirst,
                              group = "season", stroke = FALSE, fillOpacity = 0.9) %>%
    leaflet::hideGroup("season") %>%
    leaflet::addLegend("bottomright", pal = pal.season, values = ~season,
                       title = "Season") %>%
    leaflet::addLegend("topleft", pal = pal, values = ~month,
                       title = paste0(cfg$tagName, "<br>Month")) %>%
    leaflet::addPolylines(data = points_to_line(traj, long = "lng", lat = "lat"),
                          group = "path", weight = 1) %>%
    leafem::addMouseCoordinates()
    # leaflet::addLabelOnlyMarkers(lng = -62, lat = 53,
    #                     label = htmltools::HTML(as.character(htmltools::h1(cfg$tagName))),
    #                     labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE))

  # Add smoothed points and path. Hide unsmoothed points and path by default.
  if (cfg$boxcarSmooth){
    groups <- c(groups, "smoothed pnts", "smoothed path")
    m <- m %>%
      leaflet::addCircleMarkers(lng = ~smthlng, lat = ~smthlat, radius = 3,
                          color = ~pal(month), label = ~tFirst,
                          stroke = FALSE, fillOpacity = 0.75,
                          group = "smoothed pnts") %>%

      leaflet::addPolylines(data = points_to_line(traj, long = "smthlng", lat = "smthlat"),
                          group = "smoothed path", weight = 1) %>%
      leaflet::hideGroup(c("points", "path"))
  }

  # Map calibration points
  if (cfg$keepCalibPoints){
    groups <- c(groups, "calib pnts")
    pnts <- dplyr::filter(traj, src == "Calib")
    m <- m %>%
      leaflet::addCircleMarkers(lng = ~lng, lat = ~lat, data = pnts,
                                radius = 1, color = "Black",
                                label = ~tFirst, group = "calib pnts") %>%
      leaflet::hideGroup("calib pnts")
  }

  # Create and map kernel UDs
  if (cfg$createKernel) {
    message("Creating kernel UD")
    # create kernel UDSs and convert back to lat long for plotting
    conts <- suppressWarnings(do_kernel(traj, lngcol = lngcol, latcol = latcol,
                                        elev = elev, cfg = cfg,
                                        shapefolder = shapefolder)) %>%
      purrr::map(~sf::st_transform(., crs = sf::st_crs(4326)))


    groups <- c(groups, "UDs")
    ud.pal <- leaflet::colorFactor(palette = "RdYlBu",
                                   domain = factor(names(conts)))

    # Add kernel UD contours to map
    for(i in 1:length(conts)) {
      m <- leaflet::addPolygons(m, data = conts[[i]], group = "UDs",
                                color = ~ud.pal(names(conts)[i]),
                                opacity = 0.75)
    }
    m <- leaflet::addLegend(m, "bottomleft", pal = ud.pal, values = names(conts),
                       title = "UD %")
  }

  m <- leaflet::addLayersControl(m, overlayGroups = groups,
                    options = leaflet::layersControlOptions(collapsed = FALSE))

  # We only want to print the map now if we are in NOT being knitted/renderec
  if (cfg$plotMap && (is.null(getOption('knitr.in.progress')) |
                      !isTRUE(getOption('knitr.in.progress')))) {
    message("Displaying leaflet map")
    print(m)
  }

  message("All done.")
  list(posns = traj, elev = elev, light = dat,
       act = if(exists("act")) {
         act
       }else {
         NULL
       },
       calib = calib, cfg = cfg, map = m)
}

do_shapefile <- function(dat, lngcol, latcol, elev, cfg, shapefolder) {
  shp <- sf::st_as_sf(dat, coords = c(lngcol, latcol), crs = st_crs(4326), remove = FALSE )
  filename <- paste0(cfg$tagName, "_thr_", cfg$lThresh, "_elev_", round(elev, 2),
                ifelse(cfg$boxcarSmooth, paste0("_smooth", cfg$b_iter), ""))
  message(sprintf("\nCreating point shapefile: %s", filename))
  st_write(
    shp,
    dsn = shapefolder,
    layer = filename,
    driver = "ESRI Shapefile",
    delete_layer = TRUE
  )

  shp
}


# Create kernel UDs for the given percentage levels, excluding calibration
# points.
do_kernel <- function(dat, lngcol, latcol, elev, cfg, shapefolder) {
  # exclude calib points and create IDcolumn to make kernelUD() happy
  dat <- dat %>%
    dplyr::filter(src != "Calib") %>%
    dplyr::mutate(id = 1)

  # filter specified dates if given
  if (!is.na(cfg$kernelStart) && !is.na(cfg$kernelEnd)) {
    dat <- dplyr::filter(dat, tFirst >= cfg$kernelStart & tSecond <=  cfg$kernelEnd)
  }

  # Create a projection string assuming a lambert conic is good.
  # Longitude of center is mean of dat longitudes,
  # Latitudes of two standard parallells are 1/6 and 5/6 of
  # the latitudinal range of data.
  # See https://gis.stackexchange.com/questions/257153/lambert-conformal-conic-one-standard-parallel-choosing-latitude-of-origin-s
  if (is.na(cfg$projString)) {
    lats <- dat[, latcol]
    span <- (range(lats) - min(lats))[2]
    lat1 <- min(lats) + (span/6)
    lat2 <- min(lats) + ((5/6) * span)

    prjString <- paste0("+proj=lcc +lon_0=", mean(dat[, lngcol]),
                      " +lat_1=", lat1, " +lat_2=", lat2, " +ellps=GRS80")
  } else {
    prjString <- cfg$projString
  }

  message("\tConverting points to sp object")
  shp <- sf::st_as_sf(dat, coords = c(lngcol, latcol), crs = st_crs(4326)) %>%
    sf::st_transform(st_crs(prjString)) %>%
    dplyr::select(id, geometry)

  # Normally I would just pipe the former to as_Spatial() but it has been
  # causing C_stack_limit errors which usually occur due to infinite recursion so...
  #
  # Note: kernelUD only wants 1 single animal ID column
  shp <-
    sp::SpatialPointsDataFrame(st_coordinates(shp),
                               dplyr::select(shp, id) %>% sf::st_drop_geometry(),
                               proj4string = sp::CRS(prjString))

  message("\tDoing kernelUD")
  kern <- adehabitatHR::kernelUD(shp,
                                 h = ifelse(is.na(cfg$h), "href", cfg$h),
                                 grid = cfg$grid)

  # create a vector of percents
  pcts <- cfg$pcts %>%
    strsplit(",") %>%
    unlist()

  message("\tGetting volume contours")
  # Generate volume contours
  conts <- pcts %>%
    purrr::map(~ adehabitatHR::getverticeshr(kern, percent = ., unin = cfg$unin, unout = cfg$unout)) %>%
    purrr::map(sf::st_as_sf) %>%
    purrr::set_names(pcts)

  message("\tSaving UDs")
  # Save volume contours, if requested
  if (cfg$createKernelShapefile) {
    purrr::imap(conts, create_kernel_shapefile, cfg = cfg, elev = elev,
         shapefolder = shapefolder)
  }

  conts
}

create_kernel_shapefile <- function(shp, pct, cfg, elev, shapefolder) {
  filename <- paste0(cfg$tagName, "_thr_", cfg$lThresh, "_elev_", round(elev, 2),
                ifelse(cfg$boxcarSmooth, paste0("_smooth", cfg$b_iter), ""),
                "_UD_", pct)
  message(sprintf("Creating %s UD shapefile: %s", pct, filename))
  st_write(
    shp,
    dsn = shapefolder,
    layer = filename,
    driver = "ESRI Shapefile",
    delete_layer = TRUE
  )
}

# Modified from original by Carl Witthoft.
# use bfunc to specify what function to apply to the windowed
# region.
# basically must be valid function name and must accept
# single value or vector of values
# "pad" generates partial-width values at ends of x so output is
# same length as input.
# and make it optional for picky people
boxcar <- function(x, width = 5, bfunc = 'mean', pad = TRUE, anchor.ends = TRUE, ...){
  goodfunc <- try(bfunc <- get(bfunc), silent = TRUE)
  if (inherits(goodfunc,"try-error")) {
    stop('"', bfunc, '"', ' is not a known function', call. = FALSE)
  }

  # paranoid, force width to be integer
  width <- floor(width)

  # fix width given definition of window() inputs
  width <- max(0,(width - 1))

  if (width %% 2 == 1) cat('Warning: window is even length, hence asymmetric\n')

  #adjust start, end points to half-window width; keep output length right
  seqstart <- 1 - as.numeric(pad)*((width + 1) %/% 2)
  seqend <- length(x) - width + as.numeric(pad) * (width %/% 2)

  # cat(paste("seqstart: ", seqstart, " seqend: ", seqend, "\n", sep = ""))
  # flush.console()

  if (seqend < seqstart) {
    stop(' input sequence is to short for that width', call. = FALSE)
  }

  boxout <- mapply(
    function(shiftx, anchor.ends) {
      #      cat(paste("Anchor.ends = ", anchor.ends, "\n"))
      win <- window(x,max(shiftx,1), min(shiftx + width,length(x)))
      n.extras <- 0
      if (pad) {
        n.extras <- width - length(win) + 1
        if (shiftx < 1) {
          #win <- c(rep(NA, n.extras), win)
          win <- c(rep(win[1], n.extras), win)
        } else {
          # win <- c(win, rep(NA, n.extras))
          win <- c(win, rep(win[length(win)], n.extras))
        }
      }
      # Debugging
      #      cat(paste("\t seq: ", shiftx, " win: "))
      #      cat(win)
      #      cat(" n.extras: ", n.extras, "\n")

      # XXXXX this is only sensible if pad is also true
      # if anchor.ends is True then do not move the position of the first and last positions
      if (pad & anchor.ends) {
        if (shiftx == seqstart) {
          res <- win[1]           # were doing the first position - just return it
        } else if (shiftx == seqend) {
          res <- win[length(win)] # were doint the last position - just return it
        } else {
          res <- bfunc(win, ...)  # were doing an interior position - apply bfunc
        }
      } else {# no pad and/or anchor, just do bfunc
        res <- bfunc(win, ...)
      }
      return(res)
    }, seq(seqstart, seqend), MoreArgs = list(anchor.ends)
  )

  return(boxout)
}

# Adapted from here: https://rpubs.com/walkerke/points_to_line
points_to_line <- function(data, long, lat) {

  # Convert to SpatialPointsDataFrame
  sp::coordinates(data) <- c(long, lat)
  sp::SpatialLines(list(sp::Lines(list(sp::Line(data)), "id")))
}



#'@export
#'@title Plot activity data
#'
#'@description Plot activity data as measured by wet/dry switch
#'
#'@param dat \[dataframe]\cr Required. A dataframe containing columns
#'    \code{datetime} and \code{act}.
#'
#'@param type \[character]\cr Optional. The type of plot to produce: point ("p"),
#'    line ("l"), or both ("b"). Plots with lines tend to be very crowded,
#'    so it's helpful to convert the resulting plot to \code{plotly} and zoom in.
#'
#'@param maxact \[numeric]\cr Optional - default 200. The maximum value \code{act} can take.
#'    Used to scale the y axis of the plot in %.
#'
#'@details This function takes the activity data from the wet-dry sensor and
#'     plots time on the x-axis and percentage wet per interval on the y-axis.
#'     The length of the interval is computed as the time between the first two
#'     points in the activity series. This interval is simply used to label the
#'     y-axis.
#'
#' @return The \code{ggplot} invisibly.
#' @section Author: Dave Fifield
#' @examples
#'
#' res <- do_multi_geolocation(folder = here::here("Data/Tag data"),
#'                        cfgfile = here::here("Data/geolocation settings.csv"),
#'                        shapefolder = here::here("GIS/Shapefiles"),
#'                        subset = c("C4567"))
#'
#' # Point plot
#' p <- plot_activity(res$C4567$act)
#' p
#'
#' # Line and point plot
#' p <- plot_activity(res$C4567$act, type = "b")
#' p
#' library(plotly)
#' pl <- ggplotly(p)
#' pl
#'
plot_activity <- function(dat, plottype = c("p", "l", "b"),
                          acttype = c("coarse", "fine"),
                          maxact = 200) {
  plottype = match.arg(plottype)
  acttype = match.arg(acttype)
  if (acttype == "coarse") {
    int <- dat$datetime[2] - dat$datetime[1] # time between measures
    p <- switch(plottype,
        p = ggplot2::ggplot(dat, ggplot2::aes(x = datetime, y = act/maxact * 100))  +
              ggplot2::geom_point(size = 0.1),
        l = ggplot2::ggplot(dat, ggplot2::aes(x = datetime, y = act/maxact * 100))  +
              ggplot2::geom_line(),
        b = ggplot2::ggplot(dat, ggplot2::aes(x = datetime, y = act/maxact * 100))  +
              ggplot2::geom_point(size = 0.1) + ggplot2::geom_line()
      )
    p <- p + ggplot2::ylab(sprintf("Percent wet (per %d min)", as.integer(int)))
  } else { # fine scale activity data
    p <- ggplot2::ggplot(dat, aes(x = datetime, y = act)) + geom_line()
    p <- p + ggplot2::ylab("Activity sensor")
  }
  print(p)
  invisible(p)
}

# Function to return weighted mean of a single cell in a set of kernel density
# surfaces. Each column of df (actually a matrix) is the cell values for one
# kernel. So we collapse seach row into a single value.
weighted.row <- function(df, weights){
  return(weighted.mean(df, weights))
}

#' @export
#' @title Combine density surfaces
#'
#' @description Combine a list of kernel density surfaces into one, using
#'   weighting, in order to give \strong{each kernel surface equal weight} in
#'   the final product.
#'
#' @param k an object of class `estUDm`
#'
#' @param w a numeric vector of the same length as the number of kernels in `k`.
#'   This is used to weight datasets that have different sampling frequencies.
#' @param npoints a vector of the same length as the number of kernels to combine
#'   giving the number of points that that were used to create each kernel.
#' @param verbose if TRUE, extra info is output.
#' @details The intention of this function is to allow the user to combine
#'   several kernel surfaces (NOT UD contours) into a single composite kernel
#'   (which could subsequently be contoured).
#'   This problem typically arises when you have tracked multiple individual
#'   animals and would like to produce a population-wide
#'   kernel surface (and subsequent UD contour). Typically in such situations
#'   it is desired that each animal's track (or some portion of it, say, winter)
#'   contribute equally to the composite.
#'   That is, the animal track is the "sample unit", so to speak with each
#'   animal contributing equally to the whole. The problem
#'   that arises is that each animal track may have different numbers of points.
#'   Blindly throwing the points from all animals into a single kernel analysis
#'   will give more weight to animals whose track contain more points than others.
#'
#'   There are two (non-exclusive) ways in which some animals' tracks may
#'   contain more points than others:
#'
#'   \enumerate{
#'      \item each animal was at liberty for differing periods of time
#'      \item animals used different tracking technology or settings. For
#'         example, some animals carried GLS giving 2 points per day
#'         whereas others carried GPS giving 12 points per day.
#'   }
#'
#'  If you only have the first problem then set `w` to all `1`'s and
#'  the function will handle the unequal point numbers.
#'
#'  If you also have the second problem, then the `w` parameter is designed to
#'  solve this. For example, imagine you have a dataset with 2 GLS tracks and
#'  2 GPS tracks (with a sampling rate of 2 hours) and have created kernels for
#'  each animal in a single \code{estUDm} object with the two GLS kernels first
#'  follwed by the 2 GPS kernels. Since GLS provides 2 positions every day
#'  and the GPS will provide 12 positions every day, then each GLS point is
#'  "worth" 12 GPS points so we would set w = c(12, 12, 1, 1). Note that the function
#'  will still handle the unequal numbers of points per track in this case.
#'
#'
#' The code assumes that k is of class estUDm (see adehabitatHR) and that each of
#' the individual estUDs in the estUDm structure are the kernels to be combined.
#' Normally, in adehabitatHR, estUDm(s) are used to contain the separate kernels
#' for each animal, but here the elements can be any kernel that we want to
#' combine.
#'
#' Note that each kernel in k must have been estimated \strong{on the same grid}. There
#' are currently \strong{no checks} to ensure this is true.
combine_kernel_density_surfaces <- function(k,  w, npoints = NULL, verbose = FALSE) {

  if (!is.null(npoints)) {
    if (length(k) != length(w))
      stop("Length of the kernel list and weights vector are not equal.")
  } else {
    if (length(k) != length(w))
          stop("Length of the kernel list and weights vector are not equal.")
  }

	# if all surfaces are empty, return nULL
	lens <- lapply(k, length)
  if (all(lens == 0)) {
  	cat("All elements in list of density surface to be combined is/are NULL. Returning Null as combined kernel.\n")
		return(NULL)
  }

	# return the only non-null surface if there is only one
	if (sum(lens != 0) == 1) {
		cat("Only one density surface is not NULL. Returning it as combined kernel.\n")
		return(k[[lens != 0]])
	}

	# multiple (but not all) density surfaces are null. Extract the non-null ones
	# and weight appropriately
	if (any(lens == 0)) {
		cat(paste(sum(lens == 0), " density surfaces to be combined are NULL. Combining non-null ones. "), sep = "")
		k <- k[lens != 0]
		w <- w[lens != 0]
		npoints <- npoints[lens != 0]
	}

	if (verbose) {
    if (!is.null(npoints)) {
      cat(paste("Combining ",  length(k), " density surfaces with sizes ",
            paste(npoints, collapse = " "), " weighted ",
        paste(w, collapse = ":"), ".\n", sep = ""))
    } else {
     cat(paste("Combining ",  length(k), " density surfaces with sizes ",
        paste(unlist(lapply(k, function(x) attr(slot(x, "data"), "npoints"))),
          collapse = " "), " weighted ",
        paste(w, collapse = ":"), ".\n", sep = ""))
    }
	}

  # set the class properly to make estUDm2spixdf happy
  class(k) <- "estUDm"

  # convert multiple UDs to a spatial pixels df and grab the data slot
  ii <- adehabitatHR::estUDm2spixdf(k)@data

  # Get the number of points in each kernel. The "else" case is for kernels
  # that were created with my kernel "library" that stored the number
  # of points in each kernel as an attribute of the data slot.
  if (!is.null(npoints))
    pts <- npoints
  else
    pts <- plyr::laply(k, function(x) return(attr(slot(x, "data"), "npoints")))

  # compute the actual weighting needed. This is a combination of the weighting
  # supplied by the user and the number of points in each kernel to start with.
  # If the vector of weights supplied by the user are all 1's then this is just
  # the number of points in each kernel, which are required to put the pieces
  # together equitably.

  # get the combined weights
  c.weights <- pts * w

  # merge the data from each kernel weighting the means by the number of points
  # contributed by each kernel times the user supplied weighting. Note that it
  # may at first seem counter-intuitive to weight by the number of points
  # and not the inverse of that. However, the values in the cells of each kernel
  # have already been DOWN-weighted by the number of points in the track as
  # part of the kernel equation (see "Kernel methods for estimating the
  # utilization distribution in home-range studies" Worton 1989. Thus within
  # each kernel the values makes sense on a *relative* scale in comparison to
  # each other, but this does not hold across kernels. So to place each kernel's
  # cell values on an equal scale it is required to upweight each cell values
  # by it's number of points in order to be comparable between kernels.
  new.data <- apply(ii, 1, weighted.row, c.weights)

  # create a new estUD structure from the original k and replace it's density
  # surface with the newly created one
  kern_comb <- k[[1]] # just to get an estUD object
  slot(kern_comb, "data")$ud <- new.data
  attr(slot(kern_comb, "data"), "npoints") <- sum(c.weights) # Store

  return(kern_comb)
}

# found at https://stackoverflow.com/questions/44076935/use-r-to-check-and-see-if-a-file-is-open-and-force-close-it
# Check if a file is open (eg. by Excel). Returns TRUE if open. FALSE otherwise.
file.opened <- function(path) {
  suppressWarnings(
    "try-error" %in% class(
      try(file(path,
               open = "r"),
          silent = TRUE
      )
    )
  )
}
