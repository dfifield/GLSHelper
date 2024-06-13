function log_temps = read_BAS_logger_temps(filename)
% read_BAS_logger_temps(filename)
% Read in the temperature data from a BAS geologger and return in
% log_temps.
%
% Format of file is:
%
% status, date and time, datetime offset from some start time, temp
%
% Eg.
% ok,06/08/07 20:55:57,39300.872187,14.562500

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

% create an empty array of the correct size. Maximum number of 'ok' lines
% will be num_lines.
log_temps_temp = zeros(num_lines, 3);

% back to start
if(fseek(fid, 0, 'bof') < 0)
    disp(['Stage 2 of 5: ERROR: fseek to begining of temperature file: ', ferror(fid)]);
    return;
end

num_ok = 0;
% ok,06/08/07 20:55:57,39300.872187,14.562500
for i = 1:num_lines
    tline = fgetl(fid);
    if ~ischar(tline), break, end

	% Extract the status field 
	[tok, rem] = strtok(tline, ',');
	
	% only process 'ok' lines.
	if (strcmp(tok,'ok'))
        num_ok = num_ok + 1;
		
        % Extract the Date and Time
		[tok, rem] = strtok(rem(2:length(rem)), ',');
		log_temps_temp(i, 1) = datenum(tok, 'dd/mm/yy HH:MM:SS');

		% Extract time offset and ignore for now.
		[tok, rem] = strtok(rem(2:length(rem)), ',');		
		
		% Extract temperature
		log_temps_temp(i, 2) = str2double(rem(2:length(rem)));
        
        % mark as OK
        log_temps_temp(i, 3) = 1;
    else
        % mark as not ok
        log_temps_temp(i, 3) = 0;
    end
	
	% Print a status update every 100 lines processed
	if (mod(i,1000) == 0)
		fprintf('Stage 2 of 5: Read %d (of %d) lines of logger temperature data.\n', i, num_lines);
	end
end

% were there any bad lines
if (num_ok ~= num_lines)
    msgbox(['Number of temperature lines: ' num2str(num_lines) ' differs from number ok: ' num2str(num_ok)], 'Temperature data', ...
        'warn')
end

% save the ok rows for rest of package to use
log_temps = log_temps_temp((log_temps_temp(:,3) == 1), 1:2);

log_temps_temp = [];

fprintf('Stage 2 of 5: Read %d (of %d) lines of logger temperature data.\n', i, num_lines);
fclose(fid);