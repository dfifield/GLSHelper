function [found] = find_and_read_SST_file(search_date)
% find_and_read_sst_file() - look for a satellite SST file in the SST_headers 
% that covers the week including the 'date' argument and read it into the 
% global SST variable.
%
% Returns TRUE in found if a file was found and saves data in global
% variable SST. Otherwise, SST will be empty and found will be FALSE.
%

% SST is not an array since it only contains a single SST dataset from 
% a single hdf file at a time. The raw l3m_data is converted to temperature 
% and stored in SST on the fly as needed for computing index1,2,3 scores,
% according to the slope/intercept in the info structure.

global options;
global SST;	% could make this a param?
global SST_headers;
global temp_SST_filename;
global temp_compressed_SST_filename;

% initialize variables and get the list of files in the HDF folder
found = false;

% check to see if there is a currently loaded SST dataset and if it
% contains date of interest.
if (isempty(SST) == false) 
	% Does currently loaded SST file include data for "date"
	if (search_date >= SST.start_datenum && search_date <= SST.end_datenum)
		% already loaded
		found = true;
		return;
	end
end

[junk, numfiles] = size(SST_headers);

% reset SST(1) to []? Does this release memory?
SST = [];

% Process all files in SST_headers looking for one that encompasses date.
for i = 1:numfiles
		% Does this SST_header point to an SST dataset that include data for "date"
		if (search_date >= SST_headers(i).start_datenum && search_date <= SST_headers(i).end_datenum)
			fprintf('\nStage 4 of 5: Uncompressing satellite dataset covering %s to %s from file %s\n\n',...
				datestr(SST_headers(i).start_datenum),	datestr(SST_headers(i).end_datenum), ...
				SST_headers(i).filename);
			
			% copy the compressed SST file to a temporary file in current folder and uncompress it
			savedir = pwd;
			cd(options.SST_folder);
			copyfile(SST_headers(i).filename, [savedir, '\', temp_compressed_SST_filename]);
			cd(savedir);
			system(['bunzip2 -f ', temp_compressed_SST_filename]);

			% read info struct with hdfinfo			
	        SST.info = hdfinfo(temp_SST_filename);

			% make sure dates in info struct match dates in header that
			% were generated based on the filename
			start_year = double(SST.info.Attributes(1,21).Value);
			start_day = double(SST.info.Attributes(1,22).Value);
			end_year = double(SST.info.Attributes(1,23).Value);
			end_day = double(SST.info.Attributes(1,24).Value);
			
			info_start_datenum = day_to_date(start_day, start_year);
			info_end_datenum = day_to_date(end_day, end_year);
			
			% check start date
			if ((SST_headers(i).start_datenum ~= info_start_datenum))
				msgbox(['SST file: ', SST_headers(i).filename, ': start date from filename (', datestr(SST_headers(i).start_datenum), ') does not match start date from info structure (', datestr(info_start_datenum),'). Using date from info structure.'], ...
                    'Date Mismatch', 'warn');
			end
			
			% check end date
			if ((SST_headers(i).end_datenum ~= info_end_datenum))
				msgbox(['SST file: ', SST_headers(i).filename, ': end date from filename (', datestr(SST_headers(i).end_datenum), ') does not match end date from info structure (', datestr(info_end_datenum),'). Using date from info structure.'], ...
                    'Date Mismatch', 'warn');
			end
			
			% copy dates from the header structure to SST
			SST.start_datenum = SST_headers(i).start_datenum; 
			SST.end_datenum = SST_headers(i).end_datenum;
			
			% read each Scietific Data Set (SDS)
			[x, num_SDS] = size(SST.info.SDS);

			
			for d = 1:num_SDS
				dataset_name = SST.info.SDS(d).Name;
				SST.(dataset_name) = hdfread(temp_SST_filename, dataset_name);
			end

			% get the number of rows and cols in the l3m_data matrix and setup
			% the temperature matrix
			rows = SST.info.SDS(1).Dims(1,1).Size;
			cols = SST.info.SDS(1).Dims(2,1).Size;
			SST.temp = zeros(rows, cols);
			SST.temp_valid = zeros(rows, cols, 'int8');
			
			% success - lets get out of here
			found = true;
			return;
		end
	end
end

