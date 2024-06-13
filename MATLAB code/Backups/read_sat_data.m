function [numread, SST] = read_sat_data()
% read_sat_data() - read in satellite SST data and create temperature matrix
% Returns the number of satellite hdf files processed. Saves data in global
% variable SST
% 
% NO LONGER IN USE. Replaced by on the fly read process April 2008.
%
% The workspace could be saved after running this function to prevent
% having to recreate the SST data every time.

% SST is an array of structures. Each array element contains the overall "info" structure from the hdf file and 
% both the l3m_data and l3m_qual Scientific DataSets(SDS). The raw l3m_data
% is then converted to temperature and stored in SST according to the slope/intercept in the
% info structure.
%

global options;

%SST_DIR = 'C:\Documents and Settings\fifieldd\My Documents\Masters\Data\Sea Surface Temperature\HDF';
SST_DIR = options.SST_folder;
files = dir(SST_DIR);
[numfiles, x] = size(files);

num_sstfiles = 0;

% Process all files in dir
for i = 1:numfiles
    if (files(i).isdir == false) % ignore directories (ie. . and ..)
        num_sstfiles = num_sstfiles + 1;
        % files(i).name % debugging output
        sst_filename = [SST_DIR, '\', files(i).name];
        SST(num_sstfiles).info = hdfinfo(sst_filename);
        
		start_year = double(SST(1, num_sstfiles).info.Attributes(1,21).Value);
		start_day = double(SST(1, num_sstfiles).info.Attributes(1,22).Value);

		end_year = double(SST(1, num_sstfiles).info.Attributes(1,23).Value);
		end_day = double(SST(1, num_sstfiles).info.Attributes(1,24).Value);

		SST(num_sstfiles).start_datenum = addtodate(datenum([start_year, 1, 1, 0, 0, 0]), start_day, 'day');
		SST(num_sstfiles).end_datenum = addtodate(datenum([end_year, 1, 1, 0, 0, 0]), end_day, 'day');

        % read each Scietific Data Set (SDS)
        [x, num_SDS] = size(SST(num_sstfiles).info.SDS);
        
		fprintf('Reading SST dataset %d (of %d max) covering %s to %s from file %s\n', num_sstfiles, numfiles - 2, datestr(start_datenum), datestr(end_datenum), sst_filename);
        for d = 1:num_SDS
            dataset_name = SST(num_sstfiles).info.SDS(d).Name;
            SST(num_sstfiles).(dataset_name) = hdfread(sst_filename, dataset_name);
        end
        
        % get the number of rows and cols in the l3m_data matrix and setup
        % the temperature matrix
        rows = SST(num_sstfiles).info.SDS(1).Dims(1,1).Size;
        cols = SST(num_sstfiles).info.SDS(1).Dims(2,1).Size;
        SST(num_sstfiles).temp = zeros(rows, cols);
        
        % get the slope and intercept to be able to compute temperatures in
        % degrees C. The first entry in the SDS array is l3m_data
        slope = double(SST(num_sstfiles).info.SDS(1).Attributes(1,3).Value);
        intercept = double(SST(num_sstfiles).info.SDS(1).Attributes(1,4).Value);
        
        SST(num_sstfiles).temp = zeros(rows, cols);
        
        % Compute the temperatue from the sensor data and slope/intercept.
        for j = 1:rows
			if (mod(j,100) == 0)
				fprintf('Computing SST temps for row %d (of %d) of data set %d (of %d max).\n', j, rows, num_sstfiles, numfiles - 2);
			end
			
			for k = 1:cols
				if (SST(num_sstfiles).l3m_qual(j,k) == 0)
                    SST(num_sstfiles).temp(j,k) = (double(SST(num_sstfiles).l3m_data(j,k)) .* slope) + intercept;
                else
                    SST(num_sstfiles).temp(j,k) = NaN;   % temperature value for missing data
				end
			end
		end
	end
end

numread = num_sstfiles;