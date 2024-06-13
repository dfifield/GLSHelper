function read_SST_file_headers()
% read_sst_file_headers() - read all the headers from available HDF data
% files
%
global options;
global SST_headers;	

SST_DIR = options.SST_folder;
files = dir(SST_DIR);
[numfiles] = size(files,1);

SST_headers = [];
datasets = 1;
% Process all files in dir and build up header struct.
% XXX Should probably go through all files first and count how many there
% are and then dimension SST_headers to the correct size instead of growing
% it in a loop.

% Change to using native bzip file format that files are downloaded in.
% Extract start_year, etc from file name.
for i = 1:numfiles
	if (files(i).isdir == false) % ignore directories (ie. . and ..)
        % files(i).name % debugging output
		
		start_year = str2double(files(i).name(2:5));
		start_day = str2double(files(i).name(6:8));

		end_year = str2double(files(i).name(9:12));
		end_day = str2double(files(i).name(13:15));

		SST_headers(datasets).filename = files(i).name;
		SST_headers(datasets).start_datenum = day_to_date(start_day, start_year);
		SST_headers(datasets).end_datenum = day_to_date(end_day, end_year);

		fprintf('Stage 3 of 5: Read satellite header %s, covering %s to %s\n', files(i).name, datestr(SST_headers(datasets).start_datenum), ...
			datestr(SST_headers(datasets).end_datenum));
		datasets = datasets + 1;		
	end
end
end