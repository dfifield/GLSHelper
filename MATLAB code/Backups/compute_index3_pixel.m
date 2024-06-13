function index3 = compute_index3_pixel(posn, center_x, center_y, log_temp_u, log_temp_pro, ...
	num_sample, sample_size, search_width, search_height, nrows, ncols, lat_step, long_step)

global SST;

% seed the random number generator
randn('state', sum(100*clock));

% to hold the sample_size temperatures drawn at a time
sing_sample = zeros(1, sample_size);

% to hold all results
res = zeros(num_sample, sample_size);
res(:,:) = NaN;

% to hold sorted unique randomly drawn temperatures
res_u = zeros(num_sample, sample_size);
res_u(:,:) = NaN;

% to hold the proportion of samples with given temperature
res_u_pro = zeros(num_sample, sample_size);
res_u_pro(:,:) = NaN;

% to hold the proportion of NaN pixels in each of the num_sample draws
NaN_pro = zeros(1, num_sample);

% Get latitude and longitude of center of search area
[center_lat, center_long] = i_to_g(center_x, center_y, nrows, ncols, lat_step, long_step);

%[search_width, search_height] = size(search_area);
% Draw num_sample random samples of sample_size points each
for i = 1:num_sample
	num_non_nan = 0;
	sing_sample(:,:) = NaN;
	
	% draw 50 random points from the search area with bivariate normal
	% distribution around center. Values in x and y are in km offset from
	% center.
	x=normrnd(0, search_width/4, sample_size, 1); % longitude
	y=normrnd(0, search_height/4, sample_size, 1); % latitude

	% get temperatures at the drawn locations.
	for j = 1:sample_size

		% Figure out y coordinate of randomly selected pixel
		lat_offset = lat_km_to_deg(y(j));	% convert offset to degrees
		drawn_lat = center_lat + lat_offset;

		% test and deal with overflow - this should never happen since
		% search areas always start at 70 deg N. But just in case....
		if (drawn_lat > 90.0)
			drawn_lat = 90.0;
		elseif (drawn_lat < -90.0)
			drawn_lat = -90.0;
		end

		% Figure out x coordinates of randomly generated pixel
		% Get the number of degrees of longitude equivalent to y(j) kilometers at
		% this latitude.
		long_offset = long_km_to_deg(x(j), center_lat);
		drawn_long = center_long - long_offset;

		% handle roll over at 180 degrees long
		if drawn_long < -180
			drawn_long = drawn_long + 360;
		elseif drawn_long > 180
			drawn_long = drawn_long - 360;
		end
		
		% convert geographic coordinates to indices
		[drawn_x, drawn_y] = g_to_i(drawn_lat, drawn_long, nrows, ncols, lat_step, long_step);

		% get temperature at that location. Check to see if temp at that
		% location has ever been filled in from slope intercept formula. It
		% may not since the bivariate normal sample can draw elements from
		% outside the search area.
		if (SST.temp_valid(drawn_y,drawn_x) ~= 1)	
			fill_in_SST_temps(drawn_y, drawn_y, drawn_x, drawn_x, false)
		end
		
		% check for NaN and round to nearest 0.05 and keep track of how
		% many actual temperatures were drawn.
		sing_sample(j) = SST.temp(drawn_y, drawn_x);
	end

	% round to nearest 0.05. This also gets rid of NaNs.
	sing_sample = round(sing_sample(isnan(sing_sample) ~= 1)/0.05) * 0.05;
	
	% sort and store sample
	res(i,1:size(sing_sample,2)) = sort(sing_sample);
	
	% extract list of unique temperatures in sample and store in res_u.
	temps_u = unique(res(i,:));
	res_u(i,1:size(temps_u,2)) = temps_u;
	
	% compute the proportion of bad pixels (NaNs);
	NaN_pro(i) = size(find(isnan(res(i,:)) == true), 2)/sample_size;
	
	% compute proportions of each temperature in the sample.
	% XXX need to fix this since it goes around the look 50 times even if
	% all temps are NaN
	for l = 1:size(temps_u,2)
		res_u_pro(i,l) = size(find(res(i,:) == temps_u(l)), 2)/sample_size;
	end
end

% Get list of unique temperature across all draws
%sat_temps = zeros(size(unique(res(isnan(res) == false)), 1), 2);
sat_temps_u = sort(unique(res(isnan(res) == false)));

% Did we manage to draw any real temperatures? If not return -1. This can
% happen in coastal areas or areas of high cloud cover.
if size(sat_temps_u, 1) == 0
	index3 = Inf;
	return
end

% For each temperature, extract mode of proportion of that temp in a given
% draw across all draws.
sat_temps_pro = zeros(1, size(sat_temps_u, 1));
for i = 1:size(sat_temps_u, 1)
	sat_temps_pro(i) = mode(res_u_pro(res_u == sat_temps_u(i)));
end

% finally for all temperatures (both logger based and satellite based
% combined) compute squared difference between expected (sat_temps_pro) and 
% observed (log_temps_pro) proportions.

% make a combined list of all temps.
%all_temps = sort(unique([sat_temps_u; log_temp_u]));

% try using only logger temps in the sum of squares
all_temps = sort(unique(log_temp_u));

% for each temperature compute difference, square it, and add to sum.
index3 = 0;
for i = 1:size(all_temps, 1)
	pt = log_temp_pro(log_temp_u == all_temps(i));
	Ept = sat_temps_pro(sat_temps_u == all_temps(i));
	
	% if pt is empty then there were no logger samples at this temp so set
	% observed proportion to 0.
	if (isempty(pt))
		pt = 0;
	end

	% if Ept is empty then there were no random samples at this temp so set
	% expected proportion to 0.
	if (isempty(Ept))
		Ept = 0;
	end
	
	% Compute a running total sum of squares.
	index3 = index3 + ((pt - Ept) ^ 2);
end

% find the modal proportion of bad pixels and add in the squared difference
% between that and the "observered" proportion of bad temps (ie. 0)
%mod_NaN = mode(NaN_pro);
%index3 = index3 + (mod_NaN)^2;
end