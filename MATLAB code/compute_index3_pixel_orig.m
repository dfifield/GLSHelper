function index3 = compute_index3_pixel(posn, center_x, center_y, log_temp_u, log_temp_pro, search_width, search_height, nrows, ncols, lat_step, long_step)

global SST;

% seed the random number generator
randn('state', sum(100*clock));

% to hold the 50 temperatures drawn at a time
sing_sample = zeros(1, 50);

% to hold all results
res = zeros(100, 50);
res(:,:) = NaN;

% to hold sorted unique randomly drawn temperatures
res_u = zeros(100,50);
res_u(:,:) = NaN;

% to hold the proportion of samples with given temperature
res_u_pro = zeros(100,50);
res_u_pro(:,:) = NaN;


% Get latitude and longitude of center of search area
[center_lat, center_long] = i_to_g(center_x, center_y, nrows, ncols, lat_step, long_step);

%[search_width, search_height] = size(search_area);
% Draw 100 random samples of 50 points each
for i = 1:100
	num_non_nan = 0;
	sing_sample(:,:) = NaN;
	
	% draw 50 random points from the search area with bivariate normal
	% distribution around center. Values in x and y are in km offset from
	% center.
	x=normrnd(0, search_width/4, 50, 1); % longitude
	y=normrnd(0, search_height/4, 50, 1); % latitude

	% get temperatures at the 50 drawn locations.
	for j = 1:50

		% Figure out y coordinate of randomly selected pixel
		lat_offset = lat_km_to_deg(y(j));	% convert offset to degrees
		drawn_lat = center_lat + lat_offset;

		% test and deal with overflow - this should never happen since
		% search areas always start at 70 deg N. But just in case....
		if (drawn_lat > 90.0)
			drawn_lat = 90.0;
		elseif (drawn_lat < -90.0)
			drawn_lat = 90.0;
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

		%slope = double(SST(1).info.SDS(1).Attributes(1,3).Value);
		%intercept = double(SST(1).info.SDS(1).Attributes(1,4).Value);

		% get temperature at that location. Check to see if temp at that
		% location has ever been filled in from slope intercept formula. It
		% may not since the bivariate normal sample can draw elements from
		% outside the search area.
		if (SST.temp_valid(drawn_y,drawn_x) ~= 1)	
			fill_in_SST_temps(drawn_y, drawn_y, drawn_x, drawn_x, false)
		end
		
		% check for NaN and round to nearest 0.05 and keep track of how
		% many actual temperatures were drawn. Perhaps we should keep
		% drawing until 50 real temps are drawn?
		t = SST.temp(drawn_y, drawn_x);
		if (isnan(t) ~= 1)
			sing_sample(j) = round(t/0.05) * 0.05; % round to nearest 0.05
			num_non_nan = num_non_nan + 1;
		end
				
		% debugging to make sure random sample of lat/long was working
		% right.
		% sampl((i - 1) * 50 + j, 1) = drawn_lat; 
		% sampl((i - 1) * 50 + j, 2) = drawn_long;
	end

	% sort and store sample
	res(i,:) = sort(sing_sample);
	num_drawn(i) = num_non_nan;
	
	% extract list of unique temperatures in sample
	junk = unique(res(i,:));
	junk = junk(isnan(junk) == false);
	res_u(i,1:size(junk,2)) = junk;
	
	% compute proportions of each temperature in the sample
	for l = 1:size(junk,2)
		junk1 = find(res(i,:) == junk(l));
		
		res_u_pro(i,l) = size(find(res(i,:) == junk(l)), 2)/num_drawn(i);
	end
end

% Then, for each randomly
% drawn temperature, compile it's proportion in each of the 100 samples of
% 50 and coose the mode as E(psubt)

% get list of unique temperature across all 100 draws
%sat_temps = zeros(size(unique(res(isnan(res) == false)), 1), 2);
sat_temps_u = sort(unique(res(isnan(res) == false)));

% extract proportion of each temperature in all draws and find mode
% if every draw was a NaN then sat_temps_u will be empty and sat_temps_pro
% will fail to be instantiated, in which case code below will fail when it
% tries to access it......

if size(sat_temps_u, 1) == 0
	fprintf('5000 draws contained all Nan');
end


for i = 1:size(sat_temps_u, 1)
	props = res_u_pro(res_u == sat_temps_u(i));
	sat_temps_pro(i) = mode(props);
end

% finally for all temperatures (both logger based and satellite based
% combined) compute squared difference between expected and observed
% proportions.

% make a combined list of all temps.
all_temps = sort(unique([sat_temps_u; log_temp_u]));

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
	
	index3 = index3 + ((pt - Ept) ^ 2);
end
end