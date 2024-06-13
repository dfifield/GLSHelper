function SST_posns = compute_SST_lat(log_posn, log_temp, n_limit, s_limit, search_box_width, search_box_height, delta_temp, secs_per_temp)
% compute_SST_lat() - find the latitude predicted by logger temps and SSTs
% for a whole deployment. 
%
% log_temp - is array of logger temperatures.
% log_posn - array of logger positions and dates from the light data
% n_limit - northern limit of search in degrees
% s_limit - southern limit of search in degrees
% delta_temp - specifies how accurately temperatures must match
% search_box_size - size of search box in km. Used to extract the
% appropriate portion of the satellite SST grid for comparison to logger
% temps.
% secs_per_temp - number of seconds between successive temperature readings.

% Version 1: assumed all sat SST temps in variable SST are filled prior to
% being called.

% Version 2: fill in SST temps (from slope and intercept params) on the fly
% as they are needed.

global SST; % could make this a param

 a = b xxxx note SST is now a 1 x 1 struct and is read on demand need to change code below

% Need to figure out a way to do this dynamically depending on type of logger.

num_days = size(log_posn);

% Create matrix to hold corrected positions: date, lat, long
% SST_posns = zeros(num_days(1,1), 3);

% Process each day in the deployment
for day = 1:num_days(1)

	% Find all logger temps with datenums between midnight this morning and
    % midnight tonight.
    date1 = log_posn(day).date;
    date2 = addtodate(date1, 1, 'day');
	
	num_SST = size(SST);
	
	% Find an SST data set for this date if it exists. Note SST is in
	% columns.

	need to fix this.........
		
	SST_data = [];
	for dataset = 1:num_SST(2)
		% debugging;
		% fprintf('SST dataset %d from %s to %s.\n', dataset, datestr(start_datenum), datestr(end_datenum));
		
		% is the current day in this SST dataset?
		if log_posn(day).date >= SST(dataset).start_datenum && log_posn(day).date <= SST(dataset).end_datenum
			SST_data = SST(dataset);
			break;
		end
	end
	
	% did we find an SST data set for today?
	if isempty(SST_data)
		error('Could not find an SST data set for %s\n', datestr(log_posn(day).date));
	end
    
    [nrows, ncols] = size(SST_data.temp);
    lat_step = SST_data.info.Attributes(1,43).Value;
    long_step = SST_data.info.Attributes(1,44).Value;

     % get longitude of todays position
	lng = log_posn(day).long;

	% get matrix indices for starting and ending pixels, working from north to south
	[x_coord, y_start] = g_to_i(n_limit, lng, nrows, ncols, lat_step, long_step);
	[x_coord, y_stop] = g_to_i(s_limit, lng, nrows, ncols, lat_step, long_step);

    
    % Find all logger temps with datenums between midnight this morning and
    % midnight tonight.
    % find the logger temp data for the day interest. The number of temps
    % will vary by day depending upon how much time the bird was on the
    % water.
    range = find(log_temp(:,1) >= date1 & log_temp(:,1) < date2);

    %extract the logger temp data for the date of interest
    [rangesize, junk] = size(range);
    log_temp_day = log_temp(range(1):range(rangesize),:);

    % index1 and index2 are a vector of index1 scores for each pixel from the n. to s.
    % limit for this day.
    index1 = zeros(y_stop - y_start + 1, 1);
    index2 = zeros(y_stop - y_start + 1, 1);
	
    % compute index1 for each pixel at this longitude.
    for y_coord = y_start:y_stop
        iteration = y_coord - y_start + 1;
                
        % find corners of satellite SST search box for this pixel.
        [bb_x_nw, bb_y_nw, bb_x_se, bb_y_se] = get_bbox(x_coord, y_coord, ...
            search_box_width, search_box_height, nrows, ncols, lat_step, long_step);

		% XXX TODO: need to compute temperatures for any entries in the
		% search box that have not been accessed before. This is part of
		% the new approach of computing temperatures on the fly only for
		% those pixels which are actually used. Need to have a method to
		% efficiently tell if a pixel has been used before or not. perhaps a matrix of
		% booleans of the same dimensions as the temperature matrix,
		% initialized to false. if any entries in this boolean matrix within
		% search box are false when we get here, then we need to compute
		% the temperatures for those pixels. 
		%
		% Note that after the first iteration of the loop, only one row of
		% new pixels is added during each iteration. Should use this to our
		% advantage. Also, want to remember which pixels are already done
		% across days, since the same SST image is used for 8 consecutive
		% days. During first iteration check all pixels in search box,
		% subsequently just check last row (or is it first) anyway, the
		% southernmost row.
		
		% First time through loop? If so check entire sat. SST search area to make
		% sure all temperatures are filled in
		if (y_coord == y_start)
			fill_in_SST_temps(bb_x_nw, bb_y_nw, bb_x_se, bb_y_se, true);
		else % otherwise just check southernmost row
			fill_in_SST_temps(bb_x_nw, bb_y_nw, bb_x_se, bb_y_se, false);
		end
		
		% debugging
		% disp(['Search box: ', int2str(iteration)]);

		% debugging
		% disp(['Search box width (pixels): ', int2str(bb_x_se - bb_x_nw + 1), ' height: ', int2str(bb_y_se - bb_y_nw + 1)]);

        % extract the subset of satellite SST grid that is included in the bbox.
        search_area = SST_data.temp(bb_y_nw:bb_y_se, bb_x_nw:bb_x_se);

        % debugging - show map of search box 
        % imagesc(search_area); figure(gcf)

        [index1(iteration), index2(iteration)] = compute_index1and2_pixel(log_temp_day, search_area, secs_per_temp, delta_temp);
    end %for
	
	
    % find the highest index1 score. Max returns the higest item in max_val
    % and it's index in y_index_1. If there's more than one match, max
    % returns the first one.
    [max_val_1, y_index_1] = max(index1);
    
    % find all the index1 scores that tied with the highest one
    all_max_values_1 = find(index1 == index1(y_index_1));
    
    % we have num_max_values_1 tied scores
    [num_max_values_1, junk] = size(all_max_values_1);
    
	% get min, max and med of tied lats for index1
    latmax_1 = i_to_g(x_coord, y_index_1 + y_start, nrows, ncols, lat_step, long_step);
    latmin_1 = i_to_g(x_coord, max(all_max_values_1) + y_start, nrows, ncols, lat_step, long_step);
    latmed_1 = i_to_g(x_coord, median(all_max_values_1) + y_start, nrows, ncols, lat_step, long_step);

	% status output
    disp(['Day ', int2str(day), ': ', datestr(date1), ' lng:', num2str(lng), ', index1: (', num2str(max_val_1), ')',int2str(num_max_values_1(1)), ' tied; min:', num2str(latmin_1), ' max:', num2str(latmax_1), ' med: ', num2str(latmed_1)]);    
	
	if (num_max_values_1 > 1) 
		% find the highest index1 score. Max returns the higest item in max_val
		% and it's index in y_index. If there's more than one match, max
		% returns the first one.
		[max_val_2, y_index_2] = max(index2);

		% find all the index1 scores that tied with the highest one
		all_max_values_2 = find(index2 == index2(y_index_2));

		% we have num_max_values tied scores
		[num_max_values_2, junk] = size(all_max_values_2);

		% get min, max and med of tied lats for index2
		latmax_2 = i_to_g(x_coord, y_index_2 + y_start, nrows, ncols, lat_step, long_step);
		latmin_2 = i_to_g(x_coord, max(all_max_values_2) + y_start, nrows, ncols, lat_step, long_step);
		latmed_2 = i_to_g(x_coord, median(all_max_values_2) + y_start, nrows, ncols, lat_step, long_step);
	    disp(['      index2: (', num2str(max_val_2), ')',int2str(num_max_values_2(1)), ' tied; min:', num2str(latmin_2), ' max:', num2str(latmax_2), ' med: ', num2str(latmed_2)]);    
	end

	disp(' ') % force a newline
	
	SST_posns(day, 1).date = datestr(date1);
    SST_posns(day, 1).lat = latmed_1;
    SST_posns(day, 1).long = lng;
end % for

end
