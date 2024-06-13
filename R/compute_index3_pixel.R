#' Title
#'
#' @param posn
#' @param center_x
#' @param center_y
#' @param log_temp_u
#' @param log_temp_pro
#' @param num_sample
#' @param sample_size
#' @param search_width
#' @param search_height
#' @param nrows
#' @param ncols
#' @param lat_step
#' @param long_step
#'
#' @return
#' @export
#'
#' @examples
compute_index3_pixel <- function(posn, center_x, center_y, log_temp_u, log_temp_pro,
                                 num_sample, sample_size, search_width, search_height,
                                 nrows, ncols, lat_step, long_step) {

  # seed the random number generator
  set.seed(sum(100 * Sys.time()))

  # to hold the sample_size temperatures drawn at a time
  sing_sample <- rep(NA, sample_size)

  # to hold all results
  res <- matrix(NA, nrow = num_sample, ncol = sample_size)

  # to hold sorted unique randomly drawn temperatures
  res_u <- matrix(NA, nrow = num_sample, ncol = sample_size)

  # to hold the proportion of samples with given temperature
  res_u_pro <- matrix(NA, nrow = num_sample, ncol = sample_size)

  # to hold the proportion of NaN pixels in each of the num_sample draws
  NaN_pro <- rep(NA, num_sample)

  # Get latitude and longitude of center of search area
  center_lat_long <- i_to_g(center_x, center_y, nrows, ncols, lat_step, long_step)
  center_lat <- center_lat_long[1]
  center_long <- center_lat_long[2]

  # Draw num_sample random samples of sample_size points each
  for (i in 1:num_sample) {
    num_non_nan <- 0
    sing_sample <- rep(NA, sample_size)

    # draw 50 random points from the search area with bivariate normal
    # distribution around center. Values in x and y are in km offset from
    # center.
    x <- rnorm(sample_size, 0, search_width/4)  # longitude
    y <- rnorm(sample_size, 0, search_height/4)  # latitude

    # get temperatures at the drawn locations
    for (j in 1:sample_size) {
      # Figure out y coordinate of randomly selected pixel
      lat_offset <- lat_km_to_deg(y[j])  # convert offset to degrees
      drawn_lat <- center_lat + lat_offset

      # test and deal with overflow - this should never happen since
      # search areas always start at 70 deg N. But just in case....
      if (drawn_lat > 90.0) {
        drawn_lat <- 90.0
      } else if (drawn_lat < -90.0) {
        drawn_lat <- -90.0
      }

      # Figure out x coordinates of randomly generated pixel
      # Get the number of degrees of longitude equivalent to y[j] kilometers at
      # this latitude.
      long_offset <- long_km_to_deg(x[j], center_lat)
      drawn_long <- center_long - long_offset

      # handle roll over at 180 degrees long
      if (drawn_long < -180) {
        drawn_long <- drawn_long + 360
      } else if (drawn_long > 180) {
        drawn_long <- drawn_long - 360
      }

      # convert geographic coordinates to indices
      drawn_xy <- g_to_i(drawn_lat, drawn_long, nrows, ncols, lat_step, long_step)
      drawn_x <- drawn_xy[1]
      drawn_y <- drawn_xy[2]

      # get temperature at that location. Check to see if temp at that
      # location has ever been filled in from slope intercept formula. It
      # may not since the bivariate normal sample can draw elements from
      # outside the search area.
      if (SST$temp_valid[drawn_y, drawn_x] != 1) {
        fill_in_SST_temps(drawn_y, drawn_y, drawn_x, drawn_x, FALSE)
      }

      sing_sample[j] <- SST$temp[drawn_y, drawn_x]
    }

    # round to nearest 0.05. This also gets rid of NaNs.
    sing_sample <- round(sing_sample[!is.na(sing_sample)], 2)

    # sort and store sample
    res[i, 1:length(sing_sample)] <- sort(sing_sample)

    # extract list of unique temperatures in sample and store in res_u
    temps_u <- unique(res[i, ])
    res_u[i, 1:length(temps_u)] <- temps_u

    # compute the proportion of bad pixels (NaNs)
    NaN_pro[i] <- sum(is.na(res[i, ])) / sample_size

    # compute proportions of each temperature in the sample.
    # XXX need to fix this since it goes around the loop 50 times even if
    # all temps are NaN
    for (l in 1:length(temps_u)) {
      res_u_pro[i, l] <- sum(res[i, ] == temps_u[l]) / sample_size
    }
  }

  # Get list of unique temperature across all draws
  # sat_temps = zeros(size(unique(res(isnan(res) == false)), 1), 2);
  sat_temps_u <- sort(unique(res[!is.na(res)]))

  # Did we manage to draw any real temperatures? If not return Inf. This can
  # happen in coastal areas or areas of high cloud cover.
  if (length(sat_temps_u) == 0) {
    index3 <- Inf
    return(index3)
  }

  # For each temperature, extract mode of proportion of that temp in a given
  # draw across all draws.
  sat_temps_pro <- rep(NA, length(sat_temps_u))
  for (i in 1:length(sat_temps_u)) {
    sat_temps_pro[i] <- mode(res_u_pro[res_u == sat_temps_u[i]])
  }

  # finally for all temperatures (logger based)
  # compute squared difference between expected (sat_temps_pro) and
  # observed (log_temps_pro) proportions.
  #
  # OLD make a combined list of all temps both logger and satellite. This
  # doesn't make sense.
  # all_temps = sort(unique([sat_temps_u; log_temp_u]));
  #
  # use only logger temps in the list of temps for sum of squares
  all_temps <- sort(unique(log_temp_u))

  # for each temperature compute difference, square it, and add to sum.
  index3 <- 0
  for (i in 1:length(all_temps)) {
    pt <- log_temp_pro[log_temp_u == all_temps[i]]
    Ept <- sat_temps_pro[sat_temps_u == all_temps[i]]

    # if pt is empty then there were no logger samples at this temp so set
    # observed proportion to 0.
    if (length(pt) == 0) {
      pt <- 0
    }

    # if Ept is empty then there were no random samples at this temp so set
    # expected proportion to 0.
    if (length(Ept) == 0) {
      Ept <- 0
    }

    # Compute a running total sum of squares.
    index3 <- index3 + ((pt - Ept)^2)
  }

  return(index3)

  # another idea is to .....
  # find the modal proportion of bad pixels and add in the squared difference
  # between that and the "observered" proportion of bad temps (ie. 0)
  # %mod_NaN = mode(NaN_pro);
  # %index3 = index3 + (mod_NaN)^2;
}

