function fill_in_SST_temps( row_u, row_l, col_start, col_end, entire_area)
% fill_in_SST_temps() - use slope/intercept to fill in SST temperatures in
% the search area box.
%
% col_start - the westernmost (smallest) column index of search area
% col_end - the easternmost (largest) column index of search area
% row_u - the northernmost (smallest) row index of search area
% row_l - the southernmost (largest) row index of search area
% entire - check entire area or just southernmost row ie row_l

global SST;

% fill in entire area or just last row?
if (entire_area == true)
	row_start = row_u;
	row_end = row_l;
else
	row_start = row_l;
	row_end = row_start;
end

% get the slope and intercept to be able to compute temperatures in
% degrees C. The first entry in the SDS array is l3m_data
slope = double(SST(1).info.SDS(1).Attributes(1,3).Value);
intercept = double(SST(1).info.SDS(1).Attributes(1,4).Value);

% Compute the temperatue from the sensor data and slope/intercept.
for j = row_start:row_end
	for k = col_start:col_end
		
		% has temperature previously has been computed for this pixel.
		% There's probably a more efficient way to do this using find....
		if (SST.temp_valid(j,k) == 0)	
			SST.temp_valid(j,k) = 1;
			if (SST.l3m_qual(j,k) == 0)
				SST.temp(j,k) = (double(SST.l3m_data(j,k)) .* slope) + intercept;
			else
				SST.temp(j,k) = NaN;   % temperature value for missing data
			end
		end
	end
end

