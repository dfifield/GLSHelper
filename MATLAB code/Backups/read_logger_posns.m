function log_posns = read_logger_posns(filename)
% read_logger_posns(filename)
% Read in the daily positions from a geologger and return in.
%
% Format of file is:
% date, latitude, longitude

fid = 0;
[fid, message] = fopen(filename);
if fid == -1
    disp([filename, ': ', message])
    return;
end

% reset value of C in case this has been run before. Will cause length(C)
% to be correct below.
C = {};

% read in the entire position file
C = textscan(fid, '%s%f%f', 'delimiter', ',');

% Loop through C and create an array of structures, 1 element per day,
% containing date, lat and long.
num_days = length(C{1});
% log_posns = zeros(num_days,1) incorrect. How do we initialize a struct array;

for i = 1:num_days
%  Note that dates were originally provided as  dd/mm/yyyy but datestr
%  requires mm/dd/yyyy. So the regexprep was necessary.
%  log_posns(i, 1).date = datenum(regexprep(C{1}(i), '^(\d+)/(\d+)/(.*)', '$2/$1/$3'));
    log_posns(i, 1).date = datenum(C{1}(i));
    log_posns(i, 1).light_lat = C{2}(i);
    log_posns(i, 1).long = C{3}(i);
end
fprintf('Stage 1 of 5: Read %d logger positions: %s to %s from file %s\n', num_days, datestr(log_posns(1,1).date), datestr(log_posns(num_days,1).date), filename);
fclose(fid);
end
