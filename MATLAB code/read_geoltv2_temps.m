function [log_temps, secs_per_temp] = read_geoltv2_temps(filename)
% read_logger_temps(filename)
% Read in the temperature data from a geologger and return in
% log_temps.
%
% Format of file is:
% date time temperature
% Note: it might be necessary to mess with the date format string depending on
% the input file.

secs_per_temp = [];
log_temps = [];

fid = 0;
[fid, message] = fopen(filename);
if fid == -1
    disp([filename, ': ', message]);
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
log_temps = zeros(num_lines, 2);

% back to start
if(fseek(fid, 0, 'bof') < 0)
    disp(['Stage 2 of 5: ERROR: fseek to begining of temperature file: ', ferror(fid)]);
    return;
end

for i = 1:num_lines
    tline = fgetl(fid);
    if ~ischar(tline), break, end
    
	% Extract the Date and Time
	dateend = strfind(tline, '       ');
	if (isempty(dateend))
		msgbox(['Malformed temperature line: ' i], 'error');
	end
	
	tok = tline(1:dateend - 1);
	rem = tline(dateend:size(tline, 2));
	
    log_temps(i, 1) = datenum(tok, 'dd.mm.yyyy HH:MM:SS');
	
	% Extract temperature
    log_temps(i, 2) = str2double(rem(2:length(rem)));

	% Print a status update every 100 lines processed
	if (mod(i,1000) == 0)
		fprintf('Stage 2 of 5: Read %d (of %d) lines of logger temperature data.\n', i, num_lines);
	end
end

% compute time diff in secs between consecutive temp readings
if (i >= 2)
	secs_per_temp = 86400 * (log_temps(i, 1) - log_temps(i - 1, 1));
else
	msgbox(['read_geoltv2_temps: Not enough temperature readings to compute seconds per reading: ' i],'warn');
end

fprintf('Stage 2 of 5: Read %d (of %d) lines of logger temperature data.\n', i, num_lines);
fclose(fid);