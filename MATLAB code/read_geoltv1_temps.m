function raw_log_temps = read_geoltv1_temps(filename)
% read_raw_geolt_temps(filename)
% Read in the temperature data from a geolt geologger, return in
% log_temps.
%
% Format of file is a single temperature on each line:
global options;

fid = 0;
[fid, message] = fopen(filename);
if fid == -1
    disp([filename, ': ', message]);
	raw_log_temps = [];
	return;
end

% Figure out how many lines are in the file.
% There's gotta be a better way....
num_lines = 0;
while 1
    tline = fgetl(fid);
    if ~ischar(tline), break, end
    num_lines = num_lines + 1;
end

% create an empty array of the correct size 
raw_log_temps = zeros(num_lines, 2);

% back to start
if(fseek(fid, 0, 'bof') < 0)
    disp(['Stage 2 of 5: ERROR: fseek to begining of temperature file: ', ferror(fid)]);
    return;
end

% Convert start date to date vector and then to seconds since 1 Jan 1970.
[y, m, d, h, mn, s] = datevec(options.log_start);
start_date_secs = date2unixsecs(y, m, d, h, mn, s);

% Process lines of temperature data
for i = 1:num_lines
    tline = fgetl(fid);
    if ~ischar(tline), break, end
    
	% add one temp_interval (in seconds) temperature line and convert back
	% to a datenum
	if (i ~= 1)
		start_date_secs = start_date_secs + options.secs_per_temp;
	end
	
	[y, m, d, h, mn, s] = unixsecs2date(start_date_secs);
	raw_log_temps(i, 1) = datenum(y, m, d, h, mn, s);

	% Replace commas in European numbers. Ie. convert , to . (eg. 10,75 --> 10.75) if required.
	tline = strrep(tline, ',', '.');
	
	% Store temperature
    raw_log_temps(i, 2) = str2double(tline);
	
	% Print a status update every 100 lines processed
	if (mod(i,5000) == 0)
		fprintf('Stage 2 of 5: Read %d (of %d) lines of logger temperature data.\n', i, num_lines);
	end
end
%fprintf('Stage 2 of 5: date of last temperature is %s\n', 	datestr(raw_log_temps(num_lines,1)));

fprintf('Stage 2 of 5: Read %d (of %d) lines of logger temperature data.\n', i, num_lines);
fclose(fid);