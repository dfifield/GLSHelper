function main()
% setup MATLAB path to include the utils dir
%cwd = cd;
%cd 'C:\Documents and Settings\fifieldd\Desktop\Masters Briefcase\Masters\SST Correction\MATLAB\Utils'
%addutils;
%cd(cwd);
global options;
global log_temp;
global log_temp_all;
global log_posn;
global SST_headers;
global SST_posns;
global SST;
global temp_SST_filename;
global temp_compressed_SST_filename;

version = 1.0;
fprintf('SST Correction v. %.2f\n', version);

SST=[];
SST_headers = [];
temp_compressed_SST_filename = 'tempsst.bz2';
temp_SST_filename = 'tempsst';

path(path, pwd); % add current directory to path so we can find the bzip program later

% Read Positions
log_posn = read_logger_posns(options.posn_file);  % read daily positions from logger data

switch options.temp_file_type % Get Tag of selected object.
    case 'BAS'
        % no need to set log_temp_all and use extract_surface_temps since
        % BAS loggers give only surface temps by design.
        log_temp = read_BAS_logger_temps(options.temp_file); % read in BAS logger temps.
		
		% to indicate that temps are not sampled regularly
		secs_per_temp = 0;
    case 'Geoltv1'
        % Code for when Geoltv1 is selected.
        log_temp_all = read_geoltv1_temps(options.temp_file); % read in temp data from geolt datafile
        log_temp = extract_surface_temps(log_temp_all, options.delta_t, options.min_surface_time);        
		secs_per_temp = options.secs_per_temp;
    case 'Geoltv2'
        % Code for when Geoltv2 is selected. secs_per_temp is inferred from
        % temperature file since it has date info.
        [log_temp_all, secs_per_temp] = read_geoltv2_temps(options.temp_file); % read in temp data from logger    
        log_temp = extract_surface_temps(log_temp_all, options.delta_t, options.min_surface_time);        
        
    otherwise
        % Code for when there is no match.
        msgbox('Illegal tag in Temperature File Type in main.m','error', 'modal');
end

% output temperature sampling rate if constant
if (secs_per_temp ~= 0)
	fprintf('Stage 2 of 5: temperatures sampled every %d secs.\n', secs_per_temp);
end

% free up memory
log_temp_all = [];

% Get Satellite data
read_SST_file_headers();

% Compute SST positions
SST_posns = compute_SST_lat(options.output_file, log_posn, log_temp, options.n_limit, options.s_limit, options.search_width, ...
	options.search_height, options.delta_t, secs_per_temp, options.delta_lat, options.start_lat, ...
	options.start_long);


fprintf('Stage 5 of 5: output written to %s\n', options.output_file);
fprintf('Done.\n');
end