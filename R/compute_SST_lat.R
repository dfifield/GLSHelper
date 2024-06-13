
# compute_SST_lat() - find the latitude predicted by logger temps and SSTs
# for a whole deployment.
#
# filename - output file
# log_temp - is array of logger temperatures.
# log_posn - array of logger positions and dates from the light data
# n_limit - northern limit of search in degrees
# s_limit - southern limit of search in degrees
# delta_temp - specifies how accurately temperatures must match
# search_box_size - size of search box in km. Used to extract the
# appropriate portion of the satellite SST grid for comparison to logger
# temps.
# secs_per_temp - number of seconds between successive temperature readings.
# delta_lat - maximum latitudinal shift allowed between successive
# positions
# start_lat, start_long = known starting location for deployment.

# Version 1: assumed all sat SST temps in variable SST are filled prior to
# being called.

# Version 2: fill in SST temps (from slope and intercept params) on the fly
# as they are needed.

#' Title
#'
#' @param filename
#' @param log_posn
#' @param log_temp
#' @param n_limit
#' @param s_limit
#' @param search_box_width
#' @param search_box_height
#' @param delta_temp
#' @param secs_per_temp
#' @param delta_lat
#' @param start_lat
#' @param start_long
#'
#' @return
#' @export
#'
#' @examples
compute_SST_lat <- function(filename,
                            log_posn,
                            log_temp,
                            n_limit,
                            s_limit,
                            search_box_width,
                            search_box_height,
                            delta_temp,
                            secs_per_temp,
                            delta_lat,
                            start_lat,
                            start_long) {

  SST_posns <- list()

  # setup outputfile
  fid <- file(filename, 'w')

  cat(
    'OID, search_box_width, search_box_height, position_lat_limit, north_limit, south_limit, max_pixels_exam, cloud_ignored, delta_t, date, month, numtemps, mintemp, maxtemp, medtemp, HDF_start_year, HDF_start_day, HDF_end_year, HDF_end_day, lat_SST, lat_light, lat, long, index1_max, max_poss_index1, index1_num_tied, index1_latmin, index1_latmax, index1_latmed, index2_max, max_poss_index2, index2_num_tied, index2_latmin, index2_latmax, index2_latmed, min_index3, index3_num_tied, index3_latmin, index3_latmax, index3_latmed, lat_step, long_step, slope, intercept, comment\n',
    file = fid
  )

  # Need to figure out a way to do this dynamically depending on type of logger.
  num_posns <- length(log_posn)

  # initialize
  last_good_SST_lat <- 100
  #last_good_SST_lat <- start_lat # initialize first known position

  total_delta_lat <- 0 # used to accumulate latitudinal limits for search

  # Process each position in the deployment
  for (posn in 1:num_posns) {
    # create and init result fields.
    SST_posns[[posn]] <- list(
      max_val_1 = NULL,
      max_poss_index1 = NULL,
      num_max_values_1 = NULL,
      latmin_1 = NULL,
      latmax_1 = NULL,
      latmed_1 = NULL,
      max_val_2 = NULL,
      max_poss_index2 = NULL,
      num_max_values_2 = NULL,
      latmin_2 = NULL,
      latmax_2 = NULL,
      latmed_2 = NULL,
      min_val_3 = NULL,
      num_min_values_3 = NULL,
      latmin_3 = NULL,
      latmax_3 = NULL,
      latmed_3 = NULL,
      date = log_posn[[posn]]$date,
      long = log_posn[[posn]]$long,
      light_lat = log_posn[[posn]]$light_lat,
      SST_lat = 100 # default value
    )

    # set the latitude search area for this position. Add delta_lat to
    # previous total to increase search latitude until an appropriate SST
    # position is found, at which point total_delta_lat is reset to 0
    # below. we can fail to find an SST posn if we don't have any SST data
    # from either the bird or satellite for today.
    total_delta_lat <- total_delta_lat + delta_lat

    # Get date limits date1 = today, date2 = tomorrow
    date1 <- log_posn[[posn]]$date
    date2 <- date1 + days(1)
  }

  # do we have SST data for this date?
  if (!find_and_read_SST_file(date1)) {
    cat(sprintf(
      'Stage 4 of 5: Failed to find an SST dataset for %s\n',
      format(date1, '%Y-%m-%d')
    ))
    next
  }

  # initialize number of rows in SST data and the size of each pixel in
  # degrees.
  nrows <- nrow(SST$temp)
  ncols <- ncol(SST$temp)
  lat_step <- as.numeric(SST$info$Attributes[1, 43]$Value)
  long_step <- as.numeric(SST$info$Attributes[1, 44]$Value)

  # useful things to have available for output later
  SST_posns[[posn]]$lat_step <- lat_step
  SST_posns[[posn]]$long_step <- long_step
  SST_posns[[posn]]$start_year <- as.numeric(SST$info$Attributes[1, 21]$Value)
  SST_posns[[posn]]$start_day <- as.numeric(SST$info$Attributes[1, 22]$Value)
  SST_posns[[posn]]$end_year <- as.numeric(SST$info$Attributes[1, 23]$Value)
  SST_posns[[posn]]$end_day <- as.numeric(SST$info$Attributes[1, 24]$Value)
  SST_posns[[posn]]$slope <- as.numeric(SST[[1]]$info$SDS[[1]]$Attributes[1, 3]$Value)
  SST_posns[[posn]]$intercept <- as.numeric(SST[[1]]$info$SDS[[1]]$Attributes[1, 4]$Value)

  # get longitude of today's position
  lng <- log_posn[[posn]]$long

  # set north_limit and south_limit to be within delta_lat degrees of
  # last good position which may not be most recent position if we failed
  # to find a match - does this ever happen?. last_good_SST_lat is initially 100 and will remain
  # so until the first time an SST latitude is found. Until this happens,
  # just keep using the hemispheric limits.
  # XXX need an option to either use or not use the start position as first
  # good lat.
  if (last_good_SST_lat != 100) {
    SST_posns[[posn]]$north_limit <- last_good_SST_lat + total_delta_lat
    SST_posns[[posn]]$south_limit <- last_good_SST_lat - total_delta_lat
  } else {
    SST_posns[[posn]]$north_limit <- n_limit
    SST_posns[[posn]]$south_limit <- s_limit
  }

  # reset north and south limits if they grow to exceed the hemispheric limits.
  if (SST_posns[[posn]]$north_limit > n_limit) {
    SST_posns[[posn]]$north_limit <- n_limit
  }

  if (SST_posns[[posn]]$south_limit < s_limit) {
    SST_posns[[posn]]$south_limit <- s_limit
  }

  # get matrix indices for starting and ending pixels, working from north to south
  x_coord <- g_to_i(SST_posns[[posn]]$north_limit,
                    lng,
                    nrows,
                    ncols,
                    lat_step,
                    long_step)[[1]]
  y_start <- g_to_i(SST_posns[[posn]]$north_limit,
                    lng,
                    nrows,
                    ncols,
                    lat_step,
                    long_step)[[2]]
  x_coord <- g_to_i(SST_posns[[posn]]$south_limit,
                    lng,
                    nrows,
                    ncols,
                    lat_step,
                    long_step)[[1]]
  y_stop <- g_to_i(SST_posns[[posn]]$south_limit,
                   lng,
                   nrows,
                   ncols,
                   lat_step,
                   long_step)[[2]]

  # Find all logger temps with datenums between midnight this morning and
  # midnight tonight.
  # find the logger temp data for the day interest. The number of temps
  # will vary by day depending upon how much time the bird was on the
  # water.
  range <- which(log_temp[, 1] >= date1 & log_temp[, 1] < date2)

  # if there were no on surface temperature from the data logger for the
  # day then we can't procede. Skip to next day.
  if (length(range) == 0) {
    cat(
      sprintf(
        '\nStage 4 of 5: Position %d of %d: %s: no bird-recorded SSTs today, skipping day.\n',
        posn,
        num_posns[1],
        format(date1, '%Y-%m-%d')
      )
    )
    next
  }

  # extract the logger temp data for the date of interest
  log_temp_day <- log_temp[range[1]:range[length(range)], ]

  SST_posns[[posn]]$mintemp <- min(log_temp_day[, 2])
  SST_posns[[posn]]$maxtemp <- max(log_temp_day[, 2])
  SST_posns[[posn]]$medtemp <- median(log_temp_day[, 2])
  SST_posns[[posn]]$numtemps <- nrow(log_temp_day)

  # index1 and index2 are a vector of index1 scores for each pixel from the n. to s.
  # limit for this day.
  index1 <- rep(0, y_stop - y_start + 1)
  index2 <- rep(0, y_stop - y_start + 1)

  # XXX round logger temps to nearest 0.05 - this should probably be done when
  # they are read in??? or is this necessary?? Teo et al says that
  # SATELLITE temps are accurate to 0.1 deg. How is this implemented??
  # log_temp_day[, 2] <- round(log_temp_day[, 2] / 0.05) * 0.05

  # number of pixels ignore due to excess cloud cover
  SST_posns[[posn]]$cloud_ignored <- 0

  # max number of pixels to be examined for this position
  SST_posns[[posn]]$max_pixels <- y_stop - y_start + 1



  # compute index1 (and index2) for each pixel at this longitude.
  for (y_coord in y_start:y_stop) {
    iteration <- y_coord - y_start + 1

    # find corners of satellite SST search box for this pixel. Note:
    # search box will get narrower the further south we go since we
    # want a constant width on earths surface but each pixel is a fixed
    # longitude (angle) wide.
    bbox <- get_bbox(
      x_coord,
      y_coord,
      search_box_width,
      search_box_height,
      nrows,
      ncols,
      lat_step,
      long_step
    )
    bb_x_nw <- bbox$bb_x_nw
    bb_y_nw <- bbox$bb_y_nw
    bb_x_se <- bbox$bb_x_se
    bb_y_se <- bbox$bb_y_se

    # First time through loop? If so check entire sat. SST search area to make
    # sure all temperatures are filled in. Otherwise we just need to
    # fill in the next row of temperatures.
    if (iteration == 1) {
      fill_in_SST_temps(bb_y_nw, bb_y_se, bb_x_nw, bb_x_se, TRUE)
    } else {
      # otherwise just check southernmost row
      fill_in_SST_temps(bb_y_nw, bb_y_se, bb_x_nw, bb_x_se, FALSE)
    }

    # extract the subset of satellite SST grid that is included in the bbox.
    search_area <- SST$temp[bb_y_nw:bb_y_se, bb_x_nw:bb_x_se]

    # compute percentage of useless pixels in this search box. This
    # includes both cloud cover and land. Can we differentiate between
    # the two? Did Teo? The quality flag is supposed to be: 0 = good,
    # 1 = questionable, 2 = cloud, 255 = gross cloud or land.....
    #
    # XXX need to save these percentages to look up later when we
    # choose a pixel as the best one for the day to see if it was > 80%
    # cloud. Did Scott do this by hand or did the algorithm do it?
    # ...I think Teo just skipped this position all together if
    # cloud_percent > 80%
    cloud_percent <- sum(is.na(search_area)) / length(search_area)
    if (cloud_percent >= 0.8) {
      SST_posns[[posn]]$cloud_ignored <- SST_posns[[posn]]$cloud_ignored + 1
      next
    }

    #fprintf('%.2f unusable pixels\n', cloud_percent * 100);

    # debugging - show map of search box
    # imagesc(search_area); figure(gcf)

    index_result <- compute_index1and2_pixel(log_temp_day, search_area, secs_per_temp, delta_temp)
    index1[iteration] <- index_result$index1
    index2[iteration] <- index_result$index2
  }

  # find the highest index1 score. Max returns the higest item in max_val
  # and it's index in y_index_1. If there's more than one match, max
  # returns the first one.
  max_val_1_result <- max(index1)
  SST_posns[[posn]]$max_val_1 <- max_val_1_result$max_val
  y_index_1 <- max_val_1_result$index

  # maximum possible index1 score is the number of water temp readings
  # for that day times the proportion of the day corresponding to each
  # temperature reading.
  num_water_temp_day <- nrow(log_temp_day)
  if (secs_per_temp == 0) {
    SST_posns[[posn]]$max_poss_index1 <- 1 # for BAS loggers
  } else {
    SST_posns[[posn]]$max_poss_index1 <- num_water_temp_day * secs_per_temp /
      86400
  }

  # find all the index1 scores that tied with the highest one
  all_max_values_1 <- which(index1 == index1[y_index_1])

  # we have num_max_values_1 tied index1 scores
  SST_posns[[posn]]$num_max_values_1 <- length(all_max_values_1)

  # get min, max and med of tied lats for index1
  SST_posns[[posn]]$latmax_1 <- i_to_g(x_coord,
                                       y_index_1 + y_start - 1,
                                       nrows,
                                       ncols,
                                       lat_step,
                                       long_step)
  SST_posns[[posn]]$latmin_1 <- i_to_g(x_coord,
                                       max(all_max_values_1) + y_start - 1,
                                       nrows,
                                       ncols,
                                       lat_step,
                                       long_step)
  SST_posns[[posn]]$latmed_1 <- i_to_g(x_coord,
                                       median(all_max_values_1) + y_start - 1,
                                       nrows,
                                       ncols,
                                       lat_step,
                                       long_step)

  # only assign latmed if it isn't 0. Ie. if we found at least one pixel
  # with matching temperatures.
  if (SST_posns[[posn]]$max_val_1 != 0) {
    SST_posns[[posn]]$SST_lat <- SST_posns[[posn]]$latmed_1
    last_good_SST_lat <- SST_posns[[posn]]$SST_lat # save to anchor next position within delta_lat degrees
    total_delta_lat <- 0 # reset search limit offset
  }

  # status output
  cat(
    sprintf(
      'Stage 4 of 5: Position %d (of %d): %s, lat: %.2f, lng: %.2f temps(n=%d): min %.2f, max %.2f, med %.2f\n\tsearched: %.2f to %.2f degrees, %d (of %d) pixels ignored for cloud/land\n\tmax index1: %f(%.2f poss) %d tied; latmin: %.2f, latmax: %.2f, latmed: %.2f\n',
      posn,
      num_posns[1],
      format(date1, '%Y-%m-%d'),
      log_posn[posn]$light_lat,
      lng,
      SST_posns[posn]$numtemps,
      SST_posns[posn]$mintemp,
      SST_posns[posn]$maxtemp,
      SST_posns[posn]$medtemp,
      SST_posns[posn]$north_limit,
      SST_posns[posn]$south_limit,
      SST_posns[posn]$cloud_ignored,
      SST_posns[posn]$max_pixels,
      SST_posns[posn]$max_val_1,
      SST_posns[posn]$max_poss_index1,
      SST_posns[posn]$num_max_values_1,
      SST_posns[posn]$latmin_1,
      SST_posns[posn]$latmax_1,
      SST_posns[posn]$latmed_1
    )
  )

  # if more than 1 tied index1 score, then go to index 2
  if (SST_posns[posn]$num_max_values_1 > 1) {
    # Find the highest index2 score among the pixels that tied on index1.

    # Extract index2 scores and their positions within index2 for
    # pixels that tied on index1.
    index2_to_search <- cbind(all_max_values_1, index2[all_max_values_1])

    # Max returns the highest item in max_val and it's index in y_index2.
    # If there's more than one match, max returns the first one.
    SST_posns[posn]$max_val_2 <- max(index2_to_search[, 2])
    y_index_2 <- which.max(index2_to_search[, 2])

    # find all the index2 scores that tied with the highest one.
    # All_max_values_2 contains indices into index2_to_search.
    all_max_values_2 <- which(index2_to_search[, 2] == index2_to_search[y_index_2, 2])

    # we have num_max_values_2 tied scores
    SST_posns[posn]$num_max_values_2 <- length(all_max_values_2)

    # get min, max and med of tied lats for index2
    SST_posns[posn]$latmax_2 <- i_to_g(x_coord,
                                       index2_to_search[y_index_2, 1] + y_start - 1,
                                       nrows,
                                       ncols,
                                       lat_step,
                                       long_step)
    SST_posns[posn]$latmin_2 <- i_to_g(x_coord,
                                       index2_to_search[max(all_max_values_2), 1] + y_start - 1,
                                       nrows,
                                       ncols,
                                       lat_step,
                                       long_step)
    SST_posns[posn]$latmed_2 <- i_to_g(x_coord,
                                       index2_to_search[round(median(all_max_values_2)), 1] + y_start - 1,
                                       nrows,
                                       ncols,
                                       lat_step,
                                       long_step)

    # maximum possible index2 score is max_poss_index1 * number of
    # pixels in the search area with the highest index2 score. Ie.
    # highest possible index2 occurs when all pixels in search area
    # match a temperature from the logger.
    # first find the number of pixels in search box centered at
    # y_index_2.
    # then multiply by max_poss_index1
    bbox <- get_bbox(
      x_coord,
      y_start +
        index2_to_search[round(median(all_max_values_2)), 1],
      search_box_width,
      search_box_height,
      nrows,
      ncols,
      lat_step,
      long_step
    )
    num_pixels <- (bbox$bb_x_se - bbox$bb_x_nw + 1) * (bbox$bb_y_se - bbox$bb_y_nw + 1)
    SST_posns[posn]$max_poss_index2 <- SST_posns[posn]$max_poss_index1 * num_pixels

    # debugging
    # search_area <- SST.temp[bbox$bb_y_nw:bbox$bb_y_se, bbox$bb_x_nw:bbox$bb_x_se]
    # image(search_area); plot(search_area)

    cat(
      sprintf(
        '\tmax index2: %f(%.2f poss) %d tied; latmin: %.2f, latmax: %.2f, latmed: %.2f\n',
        SST_posns[posn]$max_val_2,
        SST_posns[posn]$max_poss_index2,
        SST_posns[posn]$num_max_values_2,
        SST_posns[posn]$latmin_2,
        SST_posns[posn]$latmax_2,
        SST_posns[posn]$latmed_2
      )
    )


    # only assign latmed if index2 isn't 0. Ie. if we found at least one pixel
    # with matching temperatures.
    if (SST_posns[posn]$max_val_2 != 0) {
      SST_posns[posn]$SST_lat <- SST_posns[posn]$latmed_2
      last_good_SST_lat <- SST_posns[posn]$SST_lat # save last good position to anchor next one within delta_lat
      total_delta_lat <- 0
    }
  }

  # compute index3 for this posn if needed
  if (SST_posns[[posn]]$num_max_values_2 > 1) {
    # Only search the area where index2 scores tied.
    # Specifically we don't search all the way from y_start
    # to y_stop. These could be non-contiguous.
    index3_to_search <- SST_posns[[posn]]$index2_to_search[which(SST_posns[[posn]]$all_max_values_2), 1]

    # initialize index3
    index3 <- rep(0, length(index3_to_search))

    # compute proportions of each temperature in logger sample
    log_temp_u <- sort(unique(log_temp_day[, 2]))
    log_temp_pro <- rep(0, length(log_temp_u))
    for (l in 1:length(log_temp_u)) {
      log_temp_pro[l] <- sum(log_temp_day[, 2] == log_temp_u[l]) / nrow(log_temp_day)
    }

    # compute index3 for each pixel in the latitudinal range for
    # the day.
    for (iteration in 1:length(index3_to_search)) {
      y_coord <- y_start + index3_to_search[iteration] - 1
      index3[iteration] <- compute_index3_pixel(
        posn,
        x_coord,
        y_coord,
        log_temp_u,
        log_temp_pro,
        100,
        50,
        search_box_width,
        search_box_height,
        nrows,
        ncols,
        lat_step,
        long_step
      )
    }

    # figure out how many index3 scores tied. y_index_3 is just an index
    # into the potentially non-contiguous index3 and does not give the proper absolute y_coord.
    SST_posns[[posn]]$min_val_3 <- min(index3)
    y_index_3 <- which.min(index3)

    # find all the index3 scores that tied with the lowest one
    all_min_values_3 <- which(index3 == index3[y_index_3])

    # we have num_min_values_3 tied index3 scores
    SST_posns[[posn]]$num_min_values_3 <- length(all_min_values_3)

    # get min, max and med of tied lats for index3.
    # index3_to_search gives offsets relative to y_start.
    SST_posns[[posn]]$latmax_3 <- i_to_g(x_coord,
                                         index3_to_search[y_index_3] + y_start - 1,
                                         nrows,
                                         ncols,
                                         lat_step,
                                         long_step)
    SST_posns[[posn]]$latmin_3 <- i_to_g(x_coord,
                                         index3_to_search[max(all_min_values_3)] + y_start - 1,
                                         nrows,
                                         ncols,
                                         lat_step,
                                         long_step)
    SST_posns[[posn]]$latmed_3 <- i_to_g(x_coord,
                                         index3_to_search[round(median(all_min_values_3))] + y_start - 1,
                                         nrows,
                                         ncols,
                                         lat_step,
                                         long_step)

    # Check to make sure there actually are any valid
    # index3 scores (ie. not all Inf)
    # XXX check to see if this ever happens??
    if (SST_posns[[posn]]$min_val_3 != Inf) {
      SST_posns[[posn]]$SST_lat <- SST_posns[[posn]]$latmed_3
      last_good_SST_lat <- SST_posns[[posn]]$SST_lat  # save to anchor next position within delta_lat degrees

      if ((SST_posns[[posn]]$max_val_1 != 0) &&
          (SST_posns[[posn]]$max_val_2 != 0)) {
        total_delta_lat <- 0  # reset search limit offset only if there was some matching temperature in index1 or 2
      }
    }

    # status output
    cat(
      sprintf(
        "\tindex3: %f %d tied; latmin: %.2f, latmax: %.2f, latmed: %.2f\n",
        SST_posns[[posn]]$min_val_3,
        SST_posns[[posn]]$num_min_values_3,
        SST_posns[[posn]]$latmin_3,
        SST_posns[[posn]]$latmax_3,
        SST_posns[[posn]]$latmed_3
      )
    )
  }

  cat(sprintf("\tchosen SST lat: %.2f\n\n", SST_posns[[posn]]$SST_lat))

  library(lubridate)

  # Assuming SST_posns is a list of data frames, where each data frame represents a position
  for (posn in seq_along(SST_posns)) {
    v <- as.Date(SST_posns[[posn]]$date)
    write.table(
      c(
        posn,
        options$search_width,
        options$search_height,
        options$delta_lat,
        SST_posns[[posn]]$north_limit,
        SST_posns[[posn]]$south_limit,
        SST_posns[[posn]]$max_pixels,
        SST_posns[[posn]]$cloud_ignored,
        options$delta_t,
        format(v, "%Y-%m-%d"),
        year(v),
        SST_posns[[posn]]$numtemps,
        SST_posns[[posn]]$mintemp,
        SST_posns[[posn]]$maxtemp,
        SST_posns[[posn]]$medtemp,
        SST_posns[[posn]]$start_year,
        SST_posns[[posn]]$start_day,
        SST_posns[[posn]]$end_year,
        SST_posns[[posn]]$end_day,
        SST_posns[[posn]]$SST_lat,
        SST_posns[[posn]]$light_lat,
        SST_posns[[posn]]$SST_lat,
        SST_posns[[posn]]$long,
        SST_posns[[posn]]$max_val_1,
        SST_posns[[posn]]$max_poss_index1,
        SST_posns[[posn]]$num_max_values_1,
        SST_posns[[posn]]$latmin_1,
        SST_posns[[posn]]$latmax_1,
        SST_posns[[posn]]$latmed_1,
        SST_posns[[posn]]$max_val_2,
        SST_posns[[posn]]$max_poss_index2,
        SST_posns[[posn]]$num_max_values_2,
        SST_posns[[posn]]$latmin_2,
        SST_posns[[posn]]$latmax_2,
        SST_posns[[posn]]$latmed_2,
        SST_posns[[posn]]$min_val_3,
        SST_posns[[posn]]$num_min_values_3,
        SST_posns[[posn]]$latmin_3,
        SST_posns[[posn]]$latmax_3,
        SST_posns[[posn]]$latmed_3,
        SST_posns[[posn]]$lat_step,
        SST_posns[[posn]]$long_step,
        SST_posns[[posn]]$slope,
        SST_posns[[posn]]$intercept
      ),
      file = "output.csv",
      append = TRUE,
      row.names = FALSE,
      col.names = FALSE,
      sep = ","
    )

    # reset for next posn
    index1 <- NULL
    index2 <- NULL
    index3 <- NULL
    index2_to_search <- NULL
    all_max_values_2 <- NULL
    index3_to_search <- NULL
    all_min_values_3 <- NULL
    log_temp_u <- NULL
    log_temp_pro <- NULL
  }

  # XXXX need to return SST_posns somewhere
  close(fid)
}
