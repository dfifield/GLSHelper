global SST;


[SST, nrows, ncols, lat_step, long_step] = read_single_sat_data_file('C:\Documents and Settings\fifieldd\My Documents\Lab\Funk SST\A20072092007216.L3m_8D_SST_4.hdf', false);

%[SST, nrows, ncols, lat_step, long_step] = read_single_sat_data_file('C:\Documents and Settings\fifieldd\My Documents\Masters\Data\Sea Surface Temperature\HDF\A20062092006216.L3m_8D_NSST_9.hdf', false);

% Funk: 49.76N -53.18W
search_box_width = 500; % km
search_box_height = 500; % km

% get matrix coords of center of area of interest
[x_coord, y_coord] = g_to_i(49.75801, -53.18265, nrows, ncols, lat_step, long_step);
fprintf('Coordinates of center of area of interest: x = %d, y = %d\n', x_coord, y_coord);

% get bounding box of area of interest
[col_start, row_start, col_end, row_end] = get_bbox(x_coord, y_coord,search_box_width, search_box_height, ...
	nrows, ncols, lat_step, long_step);

slope = double(SST.info.SDS(1).Attributes(1,3).Value);
intercept = double(SST.info.SDS(1).Attributes(1,4).Value);

fprintf('Calculating temperatures....');
% Compute the temperatue from the sensor data and slope/intercept.
for j = row_start:row_end
	for k = col_start:col_end
		if (SST.l3m_qual(j,k) == 0)
			SST.temp(j,k) = int16(((single(SST.l3m_data(j,k)) .* slope) + intercept) * 100);
		else
			SST.temp(j,k) = NaN;   % temperature value for missing data
		end
	end
end

SST.l3m_data = [];
SST.l3m_qual = [];
fprintf('Done');