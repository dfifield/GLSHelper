#' Title
#'
#' @param log_temp
#' @param search_area
#' @param secs_per_temp
#' @param delta
#'
#' @return
#' @export
#'
#' @examples
compute_index1and2_pixel <- function(log_temp, search_area, secs_per_temp, delta) {
  # compute_index1and2_pixel() - compute the first and second indices of match (Teo et al) for
  # a given coordinate pair under consideration.
  # log_temp - array of logger temps for the current day. Structure is (date, temp)
  # search_area - 2D array of temperatures from sat within the search area
  # for this pixel.
  # secs_per_temp - number of seconds represented by each temperature (ie. 120
  # for GeoLT)
  # delta - defines how close temps must be to be considered a match.

  # Local vars:
  # p[i] - proportion of time spent at the ith matching SST
  # Np[i] - p[i] * num_matching pixels

  # number of temperatures today
  num_today <- nrow(log_temp)

  # Sort and unique the logger and satellite temp lists. Necessary to make
  # the finds more efficient
  log_temp_sort <- sort(unique(log_temp[, 2]))
  num_uniq_log_temp <- length(log_temp_sort)

  # Sort the search_area temperatures
  sst_temp_sort <- sort(as.vector(search_area))

  # Find all matching temperatures within delta degrees between the logger
  # and the search area and save the list of matching logger temps in
  # t_match.
  k <- 0

  # p and Np will require at most num_uniq_log_temp entries
  p <- rep(0, num_uniq_log_temp)
  Np <- rep(0, num_uniq_log_temp)

  # look for each unique logger temperature in the list of sst_temps +/-
  # delta. Compute index1 and index2 scores for this pixel.
  for (i in 1:num_uniq_log_temp) {
    # find indices of all elements in sst_temp_sort that
    # match the logger temperature within delta degrees.
    found <- which((log_temp_sort[i] - delta <= sst_temp_sort) & (log_temp_sort[i] + delta >= sst_temp_sort))

    # if a match is found then add the logger temp to the list of matching
    # temperatures for this pixel and compute p[i].
    if (length(found) > 0) { # true if found is not empty
      k <- k + 1  # k is the count of matching temperatures

      # debugging - not actually used
      # t_match[k] <- log_temp_sort[i]
      # imagesc(search_area); figure(gcf)

      # count the number of times the temperature appears in the logger data on this day
      # and multiply by number fraction of the day that each temperature reading represents
      # (eg. often 2 mins for GeoLT) to get p[i]. Some loggers don't
      # sample temperature regularly(ie.BAS) so in this case p[i] is the
      # proportion of the number temperatures for today.
      num_occur <- sum(log_temp[, 2] == log_temp_sort[i])
      if (secs_per_temp != 0) {
        p[k] <- num_occur * (secs_per_temp / 86400)
      } else {
        p[k] <- num_occur / num_today
      }

      # compute the second index: Np = p(k) * Num_matching_pixels
      num_pixel <- length(found)
      Np[k] <- p[k] * num_pixel
    }
  }

  # sum up all the p[i]'s and Np[i]'s for this pixel and return as result
  psum <- sum(p)
  Npsum <- sum(Np)
  return(list(psum = psum, Npsum = Npsum))
}

