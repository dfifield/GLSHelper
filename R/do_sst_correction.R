# Matlab code from main.m
# % Read Positions
# log_posn = read_logger_posns(options.posn_file);  % read daily positions from logger data
#
# switch options.temp_file_type % Get Tag of selected object.
#     case 'BAS'
#         % no need to set log_temp_all and use extract_surface_temps since
#         % BAS loggers give only surface temps by design.
#         log_temp = read_BAS_logger_temps(options.temp_file); % read in BAS logger temps.
#
# 		% to indicate that temps are not sampled regularly
# 		secs_per_temp = 0;
#     case 'Geoltv1'
#         % Code for when Geoltv1 is selected.
#         log_temp_all = read_geoltv1_temps(options.temp_file); % read in temp data from geolt datafile
#         log_temp = extract_surface_temps(log_temp_all, options.delta_t, options.min_surface_time);
# 		secs_per_temp = options.secs_per_temp;
#     case 'Geoltv2'
#         % Code for when Geoltv2 is selected. secs_per_temp is inferred from
#         % temperature file since it has date info.
#         [log_temp_all, secs_per_temp] = read_geoltv2_temps(options.temp_file); % read in temp data from logger
#         log_temp = extract_surface_temps(log_temp_all, options.delta_t, options.min_surface_time);
#
#     otherwise
#         % Code for when there is no match.
#         msgbox('Illegal tag in Temperature File Type in main.m','error', 'modal');
# end
#
# % output temperature sampling rate if constant
# if (secs_per_temp ~= 0)
# 	fprintf('Stage 2 of 5: temperatures sampled every %d secs.\n', secs_per_temp);
# end
#
# % free up memory
# log_temp_all = [];
#
# % Get Satellite data
# read_SST_file_headers();
#
# % Compute SST positions
# SST_posns = compute_SST_lat(options.output_file, log_posn, log_temp, options.n_limit, options.s_limit, options.search_width, ...
# 	options.search_height, options.delta_t, secs_per_temp, options.delta_lat, options.start_lat, ...
# 	options.start_long);
#
#
# fprintf('Stage 5 of 5: output written to %s\n', options.output_file);
# fprintf('Done.\n');
#
#


#' @title Do SST correction for a GLS dataset
#'
#' @param options (required) List of options to control the SST correction...
#'
#' @return
#' @export
#'
#' @examples
do_sst_correction <- function(options) {

  # Stage 1: Read Positions
  posns <- read_GLS_posns(options)

  # Stage 2: Get logger temperature data

  # Stage 3: Get Satellite data

  # Stage 4: Compute SST positions

  # Stage 5: Save output file

}
