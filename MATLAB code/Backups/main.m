function main()
% setup MATLAB path to include the utils dir
%cwd = cd;
%cd 'C:\Documents and Settings\fifieldd\Desktop\Masters Briefcase\Masters\SST Correction\MATLAB\Utils'
%addutils;
%cd(cwd);
global options;
global log_temp;
global log_posn;
global SST_headers;
global SST_posns;


log_posn = read_logger_posns(options.posn_file);  % read daily positions from logger data
log_temp = read_logger_temps(options.temp_file); % read in temp data from logger

read_SST_file_headers();

SST_posns = compute_SST_lat(log_posn, log_temp, options.n_limit, options.s_limit, options.search_width, ...
	options.search_height, options.delta_t, options.secs_per_temp, options.delta_lat, options.start_lat, ...
	options.start_long);

export_SST_posns(options.output_file, SST_posns);
fprintf('Done.\n');
end