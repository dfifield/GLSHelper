#' Read GLS temperature data
#'
#' @param options
#'
#' @return
#' @export
#'
#' @examples
read_GLS_temps <- function(options){}

# Matlab code:
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
