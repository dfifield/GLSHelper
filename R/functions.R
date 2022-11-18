
#' @export
#' @title Do geolocation for multiple GLS data sets
#'
#' @description Process multiple GLS logger raw light data sets according to
#'    settings given in a configuration CSV file.
#'
#' @param folder \[character]\cr Required. The name of a folder which contains
#'   the GLS light data for one or more devices. The light data for each device
#'   must be in a separate sub-folder of \code{folder}. The name of each
#'   sub-folder must match the \code{tagName} column of the config file. See
#'   the \code{file} parameter.
#'
#' @param cfgfile \[character]\cr Required. The full pathname of a CSV configuration
#'    file. This file contains one line per GLS data set to
#'    be processed. See \code{\link{do_geolocation}} for details.
#'
#' @param shapefolder \[character]\cr optional. The full pathname to a folder
#'    where shapefiles will be stored (if requested),
#'
#' @param subset \[character]\cr Optional. A character vector of tag names to
#'    include. These must match the \code{tagName} column in the configuration
#'    file. This argument overrides the value of the \code{include} column
#'    in the configuration file.
#'
#'@details This function
#'    reads the config file specified by \code{cfgfile} - each row
#'    specifies the settings for one dataset to process.  It then
#'    calls [do.geolocation] repeatedly - once for each data set.
#'
#'    There are quite strict naming conventions for subfolders, light files, etc.
#'    See \code{\link{create_GLSHelper_folders}} for details.
#'
#'@return A list which each element being the return value of
#'    \code{\link{do_geolocation}}.
#'
#'@section Author: Dave Fifield

do_multi_geolocation <- function(folder, cfgfile, shapefolder = NULL,
                               subset = NULL) {
  cfgs <- readr::read_csv(cfgfile,
          col_types =
    readr::cols( tagName =  "c",
      include = "l",
      lightFile	 =  "c",
      lThresh	 =  "i",
      maxLightInt	 =  "i",
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
      elev =  "d",
      keepCalibPoints	 =  "l",
      calibAsk	 =  "l",
      deplAsk	 =  "l",
      createShapefile	 =  "l",
      boxcarSmooth	 =  "l",
      b_iter = "i",
      b_func	 =  "c",
      b_width	 =  "i",
      b_pad	 =  "l",
      b_w	 =  "c",
      b_na.rm	 =  "l",
      b_anchor.ends	 =  "l",
      doDistanceFilter	 =  "l",
      maxSpeed	 =  "d",
      removeOutliers	 =  "l",
      minX	 =  "d",
      maxX	 =  "d",
      minY	 =  "d",
      maxY	 =  "d",
      doStatPeriods	 =  "l",
      Xlim	 =  "c",
      Ylim	 =  "c",
      createKernel	 =  "l",
      createKernelShapefile	 =  "l",
      pcts	 =  "c",
      projString	 =  "c",
      h	 =  "d",
      unin	 =  "c",
      unout =  "c",
      grid =  "i",
      plot_map = "l")
    ) %>%
    dplyr::filter(include == TRUE) %>%
    # remove rows that are all NA, Excel likes to insert these.
    dplyr::filter(!dplyr::if_all(dplyr::everything(), is.na)) %>%
    dplyr::mutate(fallEquiStart = as.POSIXct(fallEquiStart),
             fallEquiEnd = as.POSIXct(fallEquiEnd),
             springEquiStart = as.POSIXct(springEquiStart),
             springEquiEnd = as.POSIXct(springEquiEnd),
             deplStart = as.POSIXct(deplStart),
             deplEnd = as.POSIXct(deplEnd),
             filterStart = as.POSIXct(filterStart),
             filterEnd = as.POSIXct(filterEnd),
             projString = gsub('"', "", projString)
           ) %>%
    dplyr::arrange(tagName)

  if (!is.null(subset))
    cfgs %<>% dplyr::filter(tagName %in% subset)

  cfgs %>%
    split(1:nrow(.)) %>%
    purrr::map(do_geolocation, folder = folder, shapefolder = shapefolder) %>%
    purrr::set_names(cfgs$tagName)
}

#' @export
#'
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
#'  \item{\code{light} - \[numeric] the sun elevation angle used in the
#'      calculation of positions.}
#'  \item{\code{light} - \[dataframe] the raw light data after applying optional date
#'     filtering. See xxxconfig_format.}
#'  \item{\code{act} - \[dataframe] the activity data.}
#'  \item{\code{calib} - \[dataframe] the raw light data during calibration.}
#'  \item{\code{cfg} - \[dataframe] the config settings used.}
#'  \item{\code{m} - a Leaflet map.}
#' }
#' @section Author: Dave Fifield
do_geolocation <- function(cfg, folder, shapefolder = NULL) {
  message(sprintf("\n\nProcessing tag %s", cfg$tagName))

  tagDir <- file.path(folder, cfg$tagName)
  calibFile <- file.path(tagDir, paste0(cfg$tagName," calibration twilights.RData"))
  twiFile <- file.path(tagDir, paste0(cfg$tagName, " twilights.RData"))
  calibLoc <- c(cfg$calibLong, cfg$calibLat)
  w <- as.numeric(unlist(strsplit(cfg$b_w, split = ",")))
  Xlim <- as.numeric(unlist(strsplit(cfg$Xlim, split = ",")))
  Ylim <- as.numeric(unlist(strsplit(cfg$Ylim, split = ",")))
  pcts <- as.numeric(unlist(strsplit(cfg$pcts, split = ",")))

  # read light data, implicitly assumes first row is headers
  # (it is some stuff for BAS loggers which gets ignored)
  message("Reading light data")
  alldat <- GeoLight::ligTrans(file.path(tagDir, cfg$lightFile))

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
  plot(calib$datetime, calib$light, type = "l", main = paste0(cfg$tagName, " calibration"))

  calibtwi <- GeoLight::twilightCalc(datetime = calib$datetime,
                                     light = calib$light,
                                     LightThreshold = cfg$lThresh,
                                     maxLight = cfg$maxLightInt,
                                     ask = cfg$calibAsk) %>%
    dplyr::mutate(src = "Calib")

  # if no default elev given
  if (is.na(cfg$elev)) {
    elev <- with(calibtwi, GeoLight::getElevation(tFirst, tSecond, type,
                                  known.coord = calibLoc))
    message(sprintf("Elevation angle calculated from calibration period: %.2f", elev))
  } else {
    elev = cfg$elev
    message(sprintf("Elevation angle set from config file: %.2f", elev))
  }

  # keep calibration points?
  if (cfg$keepCalibPoints) {
    calibCoord <- GeoLight::coord(calibtwi$tFirst, calibtwi$tSecond,
                                  calibtwi$type, degElevation = elev)
    calibPoints <- cbind(calibtwi, calibCoord) %>%
      purrr::set_names(c(names(calibtwi), c("lng", "lat")))
  }

  dat <- alldat

  # arbitrary date filter useful to exclude as much of at colony time as possible
  if (cfg$doDateFilter) {
    dat <- dplyr::filter(dat, datetime >= cfg$filterStart &
                           datetime <=  cfg$filterEnd)
  }

  # remove spring Equinox
  if (cfg$removeSpringEqui) {
    dat <- dplyr::filter(dat, datetime <= cfg$springEquiStart |
                           datetime >= cfg$springEquiEnd)
  }

  # remove fall Equinox
  if (cfg$removeFallEqui) {
    dat <- dplyr::filter(dat, datetime <= cfg$fallEquiStart |
                           datetime >= cfg$fallEquiEnd)
  }

  # get dawn and dusk times for deployment
  twi <- GeoLight::twilightCalc(datetime = dat$datetime,
                                light = dat$light,
                                LightThreshold = cfg$lThresh,
                                maxLight = cfg$maxLightInt,
                                ask = cfg$deplAsk) %>%
    dplyr::mutate(src = "Deployment") # mark these as deployment period

  # calculate locations
  coord <- GeoLight::coord(twi$tFirst, twi$tSecond, twi$type, degElevation = elev)

  # remove points outside specified area?
  if (cfg$removeOutliers) {
    # Remove positions way outside the study area for example on the other side of the world
    cond <- (coord[, 1] >= cfg$minX & coord[, 1] <= cfg$maxX) &
            (coord[, 2] >= cfg$minY & coord[, 2] <= cfg$maxY)
    coord <- coord[cond, ]
    twi <- twi[cond, ]
  }

  # remove unrealistic positions - not sure what distance should be
  if (cfg$doDistanceFilter) {
    filt <- NA
    filt <- GeoLight::distanceFilter(twi$tFirst, twi$tSecond, twi$type,
                    degElevation = elev, distance = cfg$maxSpeed, units = "hour")

    #Need to check this since sometimes the distance filter fails
    if (!is.na(filt)) {
      coord <- coord[filt,]
      twi <- twi[filt,]
    }
  }

  # get stationary periods?
  if (cfg$doStatPeriods) {
    site <- GeoLight::changeLight(twi$tFirst, twi$tSecond, twi$type,
                              rise.prob = 0.1, set.prob = 0.1, days = 5)$site
    GeoLight::siteMap(coord, site, xlim = Xlim, ylim = Ylim)
  }

  # create trajectory
  traj <- cbind(twi, coord) %>%
    purrr::set_names(c(names(twi), c("lng", "lat"))) %>%
    dplyr::filter(!is.na(lat))

  # smoothing - iteratively smooth cfg$b_iter times
  if (cfg$boxcarSmooth) {
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
                                   sep = "-"))
  sprequi <- lubridate::ymd(paste(lubridate::year(cfg$springEquiStart), 3, 21,
                                  sep = "-"))

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
                        label = "Calibration location", group = "calib loc") %>%
    leaflet::addMarkers(lng = cfg$deplLong, lat = cfg$deplLat,
                        label = "Deployment location",
                        group = "deploy loc") %>%
    leaflet::setView(mean(na.omit(traj$lng)), mean(na.omit(traj$lat)), zoom = 5) %>%
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
    leaflet::addLegend("topleft", pal = pal, values = ~month, title = "Month") %>%
    leaflet::addPolylines(data = points_to_line(traj, long = "lng", lat = "lat"),
                          group = "path", weight = 1) %>%
    leafem::addMouseCoordinates() %>%
    leaflet::addLabelOnlyMarkers(lng = -62, lat = 53,
                        label = htmltools::HTML(as.character(htmltools::h1(cfg$tagName))),
                        labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE))

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

    # create kernel UDSs and convert back to lat long for plotting
    conts <- suppressWarnings(do_kernel(traj, lngcol = lngcol, latcol = latcol,
                                        elev = elev, cfg = cfg,
                                        shapefolder = shapefolder)) %>%
      purrr::map(~sp::spTransform(., CRSobj = sp::CRS("+proj=longlat +ellps=WGS84")))


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

  if (cfg$plot_map && !isTRUE(getOption('knitr.in.progress'))) {
    print(m)
  }

  list(posns = traj, elev = elev, light = dat,
       act = if(exists("act")) {
         act
       }else {
         NULL
       },
       calib = calib, cfg = cfg, map = m)
}

do_shapefile <- function(dat, lngcol, latcol, elev, cfg, shapefolder) {
  # create spatialpointsdataframe
  shp <- sp::SpatialPointsDataFrame(cbind(dat[, lngcol], dat[, latcol]),
                                    data = dat,
                                    proj4string = sp::CRS("+proj=longlat"))

  # create shapefile
  if (cfg$createShapefile) {
    filename <- paste0(cfg$tagName, "_thr_", cfg$lThresh, "_elev_", round(elev, 2),
                  ifelse(cfg$boxcarSmooth, paste0("_smooth", cfg$b_iter), ""))
    message(sprintf("\nCreating point shapefile: %s", filename))
    suppressWarnings(rgdal::writeOGR(obj = shp, dsn = shapefolder, layer = filename,
                    driver = "ESRI Shapefile", overwrite_layer = T))
  }
  shp
}


# Create kernel UDs for the given percentage levels, excluding calibration
# points.
do_kernel <- function(dat, lngcol, latcol, elev, cfg, shapefolder) {
  # exclude calib points and create IDcolumn to make kernelUD() happy
  dat <- dat %>%
    dplyr::filter(src != "Calib") %>%
    dplyr::mutate(id = 1)

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

  shp <- sp::SpatialPointsDataFrame(cbind(dat[, lngcol], dat[, latcol]),
                                    data = dat,
                                    proj4string = sp::CRS("+proj=longlat")) %>%
    sp::spTransform(sp::CRS(prjString))

  # kernelUD only wants 1 single animal ID column
  shp@data <- dplyr::select(dat, id)
  kern <- adehabitatHR::kernelUD(shp, h = cfg$h, grid = cfg$grid)


  # create a vector of percents
  pcts <- cfg$pcts %>%
    strsplit(",") %>%
    unlist()

  # Generate volume contours
  conts <- pcts %>%
    purrr::map(~ adehabitatHR::getverticeshr(kern, percent = ., unin = cfg$unin, unout = cfg$unout)) %>%
    purrr::set_names(pcts)


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
  suppressWarnings(rgdal::writeOGR(obj = shp, dsn = shapefolder, layer = filename,
                  driver = "ESRI Shapefile", overwrite_layer = T))
}

#' @noRd
#'@title Create GLShelper folder structure
#'
#'@description blah
#'
#'@param root \[character]\cr Required. The root folder of your data analysis
#'    project.
#'
#'@details GLSHelper is obnoxiously opinionated about the folder structure
#'   containing your logger data and any generated outputs, shapefiles, etc.
#'
#'
#'```
#' -- root
#'   |__Data
#'     |__geolocation settings.csv
#'     |__Tag data
#'        |__MK3005 025
#'          ...
#'   |__GIS
#'      |__Shapefiles
#'      |__Images
#'```
#'
#'   This function checks to see if sub-folders \code{Data, Data\\Tag Data, GIS,
#'   GIS\\Shapefiles} and \code{GIS\\Images} exist and creates them if not.
#'
#' @return Nothing.
#' @section Author: Dave Fifield
#' @examples
#'
#' # Create folder structure in root of RStudio project
#' create_GLSHelper_folders(here::here())
create_GLSHelper_folders <- function(root = NULL) {
  if (is.null(root)) stop("create_GLSHelper_folders: root is null, but is required!")

  if (!dir.exists(root))
    stop(sprintf("create_GLSHelper_folders: root folder '%s' does not exist.",
                 root))

  folders <- c(file.path(root, "Data"),
               file.path(root, "Data/Tag data"),
               file.path(root, "GIS"),
               file.path(root, "GIS/Shapefiles"),
               file.path(root, "GIS/Images"))

  folders %>%
    purrr::walk(function(folder){
        if (!dir.exists(folder)) {
          message(sprintf("Creating folder '%s'", folder))
          dir.create(folder)
        }
    }
  )
}


## Used to create the folder tree structure for documentation
## create_GLSHelper_folders()
##
## Taken from https://gist.github.com/jennybc/2bf1dbe6eb1f261dfe60
##
## quick-and-dirty ersatz Unix tree command in R
## inspired by this one-liner:
## ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'
## found here (among many other places):
## http://serverfault.com/questions/143954/how-to-generate-an-ascii-representation-of-a-unix-file-hierarchy
twee <- function(path = getwd(), level = Inf) {

  fad <-
    list.files(path = path, recursive = TRUE,no.. = TRUE, include.dirs = TRUE)

  fad_split_up <- strsplit(fad, "/")

  too_deep <- lapply(fad_split_up, length) > level
  fad_split_up[too_deep] <- NULL

  jfun <- function(x) {
    n <- length(x)
    if(n > 1)
      x[n - 1] <- "|__"
    if(n > 2)
      x[1:(n - 2)] <- "   "
    x <- if(n == 1) c("-- ", x) else c("   ", x)
    x
  }
  fad_subbed_out <- lapply(fad_split_up, jfun)

  cat(unlist(lapply(fad_subbed_out, paste, collapse = "")), sep = "\n")
}

# Modified from original by Carl Witthoft.
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

# Taken from here: https://rpubs.com/walkerke/points_to_line
points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {

  # Convert to SpatialPointsDataFrame
  sp::coordinates(data) <- c(long, lat)

  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }

  # If there is only one path...
  if (is.null(id_field)) {

    lines <- sp::SpatialLines(list(sp::Lines(list(sp::Line(data)), "id")))

    return(lines)

    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {

    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])

    sp_lines <- sp::SpatialLines(list(sp::Lines(list(sp::Line(paths[[1]])), "line1")))

    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- sp::SpatialLines(list(sp::Lines(list(sp::Line(paths[[p]])), id)))
      sp_lines <- maptools::spRbind(sp_lines, l)
    }

    return(sp_lines)
  }
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
