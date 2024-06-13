function [found] = find_and_read_SST_file(search_date)
% find_and_read_sst_file() - look for a satellite SST file in the SST_headers 
% that covers the week including the 'date' argument and read it into the 
% global SST variable.
%
% Returns TRUE in found if a file was found and saves data in global
% variable SST. Otherwise, SST will be empty and found will be FALSE.
%

% SST was previosuly an array of structures. Each array element contained 
% the overall "info" structure from the hdf file and 
% both the l3m_data and l3m_qual Scientific DataSets(SDS). The raw l3m_data
% is then converted to temperature and stored in SST according to the slope/intercept in the
% info structure.
%
% Now SST is not an array since it only contains a single SST dataset from 
% a single hdf file at a time. The raw l3m_data is converted to temperature 
% and stored in SST on the fly as needed for computing index1,2,3 scores,
% according to the slope/intercept in the info structure.

global options;
global SST;	% could make this a param?
global SST_headers;

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

			% copy info struct and start and end dates from SST_header to
			% SST
			SST.info = SST_headers(i).info;
			SST.start_datenum = SST_headers(i).start_datenum;
			SST.end_datenum = SST_headers(i).end_datenum;
			
			% read each Scietific Data Set (SDS)
			[x, num_SDS] = size(SST.info.SDS);

			fprintf('\nStage 4 of 5: Reading SST dataset covering %s to %s from file %s\n\n',...
				datestr(SST.start_datenum),	datestr(SST.end_datenum), SST_headers(i).info.Filename);
			
			for d = 1:num_SDS
				dataset_name = SST.info.SDS(d).Name;
				SST.(dataset_name) = hdfread(SST_headers(i).info.Filename, dataset_name);
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

