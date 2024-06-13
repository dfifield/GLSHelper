function read_SST_file_headers()
% find_and_read_sst_file() - read all the headers from available HDF data
% files
%
global options;
global SST_headers;	

SST_DIR = options.SST_folder;
files = dir(SST_DIR);
[numfiles, x] = size(files);

SST_headers = [];
datasets = 1;
% Process all files in dir looking for one that encompasses date.
% XXX Should probably go through all files first and count how many there
% are and then dimension SST_headers to the correct size instead of growing
% it in a loop.
for i = 1:numfiles
	if (files(i).isdir == false) % ignore directories (ie. . and ..)
        % files(i).name % debugging output

		sst_filename = [SST_DIR, '\', files(i).name];
        SST_headers(datasets).info = hdfinfo(sst_filename);
        
		start_year = double(SST_headers(datasets).info.Attributes(1,21).Value);
		start_day = double(SST_headers(datasets).info.Attributes(1,22).Value);

		end_year = double(SST_headers(datasets).info.Attributes(1,23).Value);
		end_day = double(SST_headers(datasets).info.Attributes(1,24).Value);

		SST_headers(datasets).start_datenum = day_to_date(start_day, start_year);
		SST_headers(datasets).end_datenum = day_to_date(end_day, end_year);

		fprintf('Stage 3 of 5: Read satellite header %s, covering %s to %s\n', files(i).name, datestr(SST_headers(datasets).start_datenum), ...
			datestr(SST_headers(datasets).end_datenum));
		datasets = datasets + 1;		
	end
end
end