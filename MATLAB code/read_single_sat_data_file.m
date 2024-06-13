function [SST, rows, cols, lat_step, long_step] = read_single_sat_data_file(sst_filename, convert)
% read_single_sat_data_file() - read in satellite SST data and create temperature matrix
% convert - boolean. convert sensor values to temperatures?
% 
SST = []; % necessary? Helpful?

SST.info = hdfinfo(sst_filename);
lat_step = SST.info.Attributes(1,43).Value;
long_step = SST.info.Attributes(1,44).Value;

% read each Scietific Data Set (SDS)
num_SDS = size(SST.info.SDS, 2);
for d = 1:num_SDS
	fprintf('Reading Scientific data set %d of %d\n', d, num_SDS);
	dataset_name = SST.info.SDS(d).Name;
	SST.(dataset_name) = hdfread(sst_filename, dataset_name);
end

% get the number of rows and cols in the l3m_data matrix and setup
% the temperature matrix
rows = SST.info.SDS(1).Dims(1,1).Size;
cols = SST.info.SDS(1).Dims(2,1).Size;

SST.temp = zeros(rows, cols, 'int16');

% Compute the temperatue from the sensor data and slope/intercept if requested.
if convert == true
	% get the slope and intercept to be able to compute temperatures in
	% degrees C. The first entry in the SDS array is l3m_data
	slope = double(SST.info.SDS(1).Attributes(1,3).Value);
	intercept = double(SST.info.SDS(1).Attributes(1,4).Value);

	for j = 1:rows
		if (mod(j,100) == 0)
			fprintf('Computing SST temps for row %d (of %d) of data set.\n', j, rows);
		end

		for k = 1:cols
			if (SST.l3m_qual(j,k) == 0)
				SST.temp(j,k) = (double(SST.l3m_data(j,k)) .* slope) + intercept;
			else
				SST.temp(j,k) = NaN;   % temperature value for missing data
			end
		end
	end
end
end