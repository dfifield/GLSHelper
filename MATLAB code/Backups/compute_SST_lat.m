function SST_posns = compute_SST_lat(log_posn, log_temp, n_limit, s_limit, search_box_width, search_box_height,...
	delta_temp, secs_per_temp, delta_lat, start_lat, start_long)
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
% delta_lat - maximum latitudinal shift allowed between successive
% positions
% start_lat, start_long = known starting location for deployment.

% Version 1: assumed all sat SST temps in variable SST are filled prior to
% being called.

% Version 2: fill in SST temps (from slope and intercept params) on the fly
% as they are needed.

global SST; % could make this a param
global SST_posns;

SST_posns = [];

% Need to figure out a way to do this dynamically depending on type of logger.
num_posns = size(log_posn);

% XXX Create matrix to hold corrected positions: date, lat, long
% SST_posns = zeros(num_posns(1,1), 3);

% initialize
%last_good_SST_lat = 100;
last_good_SST_lat = start_lat; % initialize first known position

total_delta_lat = 0;	% used to accumulate latitudinal limits for search

% Process each position in the deployment
for posn = 1:num_posns(1)
	% set the latitude search area for this position. Add delta_lat to
	% previous total to increase search latitude until an appropriate SST
	% position is found, at which point total_delta_lat is reset to 0
	% below.
	total_delta_lat = total_delta_lat + delta_lat;
	
	% Get data limits date1 = today, date2 = tomorrow
    date1 = log_posn(posn).date;
    date2 = addtodate(date1, 1, 'day');

	% initialize results
	SST_posns(posn).date = log_posn(posn).date;
	SST_posns(posn).long = log_posn(posn).long;
	SST_posns(posn).light_lat = log_posn(posn).light_lat;
	SST_posns(posn).SST_lat = 100;	 % default value
		
	% do we have SST data for this date?
	if (find_and_read_SST_file(date1) == false)
		fprintf('Stage 4 of 5: Failed to find an SST dataset for %s\n', datestr(date1));
		continue;
	end

	% initialize number of rows in SST data and the size of each pixel in
	% degrees.
	[nrows, ncols] = size(SST.temp);
    lat_step = SST.info.Attributes(1,43).Value;
    long_step = SST.info.Attributes(1,44).Value;

	% useful things to have available for output later
	SST_posns(posn).lat_step = lat_step;
	SST_posns(posn).long_step = long_step;
	SST_posns(posn).start_year = SST.info.Attributes(1,21).Value;
	SST_posns(posn).start_day = SST.info.Attributes(1,22).Value;
	SST_posns(posn).end_year = SST.info.Attributes(1,23).Value;
	SST_posns(posn).end_day = SST.info.Attributes(1,24).Value;
	SST_posns(posn).slope = SST(1).info.SDS(1).Attributes(1,3).Value;
	SST_posns(posn).intercept = SST(1).info.SDS(1).Attributes(1,4).Value;

    % get longitude of todays position
	lng = log_posn(posn).long;
	
	% set north_limit and south_limit to be within delta_lat degrees of
	% last good position which may not be most recent position if we failed
	% to find a match. last_good_SST_lat is initially 100 and will remain
	% so until the first time an SST latitude is found. Until this happens,
	% just keep using the hemisperic limits.
	if (last_good_SST_lat ~= 100)
		north_limit = last_good_SST_lat + total_delta_lat;
		south_limit = last_good_SST_lat - total_delta_lat;
	else
		north_limit = n_limit;
		south_limit = s_limit;
	end
	
	% reset north and south limits if they grow to exceed the hemispheric limits.
	if north_limit > n_limit
		north_limit = n_limit;
	end
	
	if south_limit < s_limit
		south_limit = s_limit;
	end

	% get matrix indices for starting and ending pixels, working from north to south
	[x_coord, y_start] = g_to_i(north_limit, lng, nrows, ncols, lat_step, long_step);
	[x_coord, y_stop] = g_to_i(south_limit, lng, nrows, ncols, lat_step, long_step);

    % Find all logger temps with datenums between midnight this morning and
    % midnight tonight.
    % find the logger temp data for the day interest. The number of temps
    % will vary by day depending upon how much time the bird was on the
    % water.
    range = find(log_temp(:,1) >= date1 & log_temp(:,1) < date2);

	% if there were no on surface temperature from the data logger for the
	% day then we can't procede. Skip to next day.
	if (isempty(range))
		fprintf('\nStage 4 of 5: Position %d: %s: no bird-recorded SSTs today, skipping day.\n', posn, datestr(date1));
		continue;
	end
	
    %extract the logger temp data for the date of interest
    [rangesize] = size(range, 1);
    log_temp_day = log_temp(range(1):range(rangesize),:);

	SST_posns(posn).mintemp = min(log_temp_day(:,2));
	SST_posns(posn).maxtemp = max(log_temp_day(:,2));
	SST_posns(posn).medtemp = median(log_temp_day(:,2));
	SST_posns(posn).numtemps = size(log_temp_day, 1);
	
	% index1 and index2 are a vector of index1 scores for each pixel from the n. to s.
    % limit for this day.
    index1 = zeros(y_stop - y_start + 1, 1);
    index2 = zeros(y_stop - y_start + 1, 1);

	
	% XXX round logger temps to nearest 0.05 - this should probably be done when
	% they are read in??? or is this necessary??
	log_temp_day(:,2) = round(log_temp_day(:,2) / 0.05) * 0.05;

    % compute index1 (and index2) for each pixel at this longitude.
    for y_coord = y_start:y_stop

		iteration = y_coord - y_start + 1;
        
        % find corners of satellite SST search box for this pixel. Note:
        % search box will get narrower the further south we go since we
        % want a constant width on earths surface but each pixel is a fixed
        % longitude (angle) wide.
        [bb_x_nw, bb_y_nw, bb_x_se, bb_y_se] = get_bbox(x_coord, y_coord, ...
            search_box_width, search_box_height, nrows, ncols, lat_step, long_step);

		% First time through loop? If so check entire sat. SST search area to make
		% sure all temperatures are filled in. Otherwise we just need to
		% fill in the next row of temperatures.
		if (iteration == 1)
			fill_in_SST_temps(bb_y_nw, bb_y_se, bb_x_nw, bb_x_se, true);
		else % otherwise just check southernmost row
			fill_in_SST_temps(bb_y_nw, bb_y_se, bb_x_nw,  bb_x_se, false);
		end

        % extract the subset of satellite SST grid that is included in the bbox.
		search_area = [];
		search_area = SST.temp(bb_y_nw:bb_y_se, bb_x_nw:bb_x_se);

		% compute percentage of useless pixels in this search box. This
		% includes both cloud cover and land. Can we differentiate between
		% the two? Did Teo? 
		%
		% XXX need to save these percentages to look up later when we
		% choose a pixel as the best one for the day to see if it was > 80%
		% cloud. Did Scott do this by hand or did the algorithm do it?
		cloud_percent = numel(find(isnan(search_area)))/numel(search_area);
		
        % debugging - show map of search box 
        % imagesc(search_area); figure(gcf)
		
	    [index1(iteration), index2(iteration)] = compute_index1and2_pixel(log_temp_day, search_area, secs_per_temp, delta_temp);
	end %for
	
    % find the highest index1 score. Max returns the higest item in max_val
    % and it's index in y_index_1. If there's more than one match, max
    % returns the first one.
    [SST_posns(posn).max_val_1, y_index_1] = max(index1);
    
	% maximum possible index1 score is the number of water temp readings
	% for that day times the proportion of the day corresponding to each
	% temperature reading.
	num_water_temp_day = size(log_temp_day, 1);
	SST_posns(posn).max_poss_index1 = num_water_temp_day * secs_per_temp/86400;
	
    % find all the index1 scores that tied with the highest one
    all_max_values_1 = find(index1 == index1(y_index_1));
    
    % we have num_max_values_1 tied index1 scores
    SST_posns(posn).num_max_values_1 = size(all_max_values_1, 1);
    
	% get min, max and med of tied lats for index1
    SST_posns(posn).latmax_1 = i_to_g(x_coord, y_index_1 + y_start, nrows, ncols, lat_step, long_step);
    SST_posns(posn).latmin_1 = i_to_g(x_coord, max(all_max_values_1) + y_start, nrows, ncols, lat_step, long_step);
    SST_posns(posn).latmed_1 = i_to_g(x_coord, median(all_max_values_1) + y_start, nrows, ncols, lat_step, long_step);

	% only assign latmed if it isn't 0. Ie. if we found at least one pixel
	% with matching temperatures.
	if (SST_posns(posn).max_val_1 ~= 0)	
		SST_posns(posn).SST_lat = SST_posns(posn).latmed_1;
		last_good_SST_lat = SST_posns(posn).SST_lat;	% save to anchor next position within delta_lat degrees.
		total_delta_lat = 0;		% reset search limit offset
	end

	% status output
	fprintf('Stage 4 of 5: Position %d: %s, lng: %f temps(n=%d): min %.2f, max %.2f, med %.2f\n\tsearched: %.2f to %.2f degrees\n\tmax index1: %f(%f poss) %d tied; latmin: %.2f, latmax: %.2f, latmed: %.2f\n', ...
		posn, datestr(date1), lng, SST_posns(posn).numtemps, SST_posns(posn).mintemp, SST_posns(posn).maxtemp, ...
		SST_posns(posn).medtemp, north_limit, south_limit, SST_posns(posn).max_val_1, SST_posns(posn).max_poss_index1, ...
		SST_posns(posn).num_max_values_1, SST_posns(posn).latmin_1, SST_posns(posn).latmax_1, ...
		SST_posns(posn).latmed_1);    
	
	% if more than 1 tied index1 score, then go to index 2
	if (SST_posns(posn).num_max_values_1 > 1) 
		% Find the highest index2 score among the pixels that tied on index1. 
		
		% XXX Need to fix this so it only considers pixels that tied on
		% index1.
		% extract index2 scores and their positions within index2 for
		% pixels that tied on index1.
		
		
		% Max returns the highest item in max_val and it's index in y_index2. 
		% If there's more than one match, max returns the first one.
		[SST_posns(posn).max_val_2, y_index_2] = max(index2);

		
		% find all the index2 scores that tied with the highest one
		all_max_values_2 = find(index2 == index2(y_index_2));

		% we have num_max_values tied scores
		SST_posns(posn).num_max_values_2 = size(all_max_values_2, 1);

		% get min, max and med of tied lats for index2
		SST_posns(posn).latmax_2 = i_to_g(x_coord, y_index_2 + y_start, nrows, ncols, lat_step, long_step);
		SST_posns(posn).latmin_2 = i_to_g(x_coord, max(all_max_values_2) + y_start, nrows, ncols, lat_step, long_step);
		SST_posns(posn).latmed_2 = i_to_g(x_coord, median(all_max_values_2) + y_start, nrows, ncols, lat_step, long_step);

		% maximum possible index2 score is max_poss_index1 * number of
		% pixels in the search area with the highest index2 score. Ie.
		% highest possible index2 occurs when all pixels in search area
		% match a temperature from the logger.
		% first find the number of pixels in search box centered at
		% y_index_2.
		% then multiply by max_poss_index1
		[bb_x_nw, bb_y_nw, bb_x_se, bb_y_se] = get_bbox(x_coord, y_start + median(all_max_values_2), ...
            search_box_width, search_box_height, nrows, ncols, lat_step, long_step);		
		num_pixels = (bb_x_se - bb_x_nw + 1) * (bb_y_se - bb_y_nw + 1);
		SST_posns(posn).max_poss_index2 = SST_posns(posn).max_poss_index1 * num_pixels;
        
		% debugging
		%search_area = SST.temp(bb_y_nw:bb_y_se, bb_x_nw:bb_x_se);
		%imagesc(search_area); figure(gcf);
		
		fprintf('\tindex2: %f(%f poss) %d tied; latmin: %.2f, latmax: %.2f, latmed: %.2f\n', ...
			SST_posns(posn).max_val_2, SST_posns(posn).max_poss_index2, SST_posns(posn).num_max_values_2, ...
			SST_posns(posn).latmin_2, SST_posns(posn).latmax_2, SST_posns(posn).latmed_2);    


		% only assign latmed if it isn't 0. Ie. if we found at least one pixel
		% with matching temperatures.
		if (SST_posns(posn).max_val_2 ~= 0)	
			SST_posns(posn).SST_lat = SST_posns(posn).latmed_2;
			last_good_SST_lat = SST_posns(posn).SST_lat; % save last good position to anchor next one within delta_lat
			total_delta_lat = 0;
		end

		% compute index3 for this posn if needed
		if (SST_posns(posn).num_max_values_2 > 1)
		    index3 = zeros(y_stop - y_start + 1, 1);

			%compute proportions of each temperature in logger sample
			log_temp_u = unique(sort(log_temp_day(:,2)));
			log_temp_pro = zeros(1, size(log_temp_u, 1));
			for l = 1:size(log_temp_u,1)
				log_temp_pro(l) = size(find(log_temp_day == log_temp_u(l)),1)/size(log_temp_day,1);
			end

			% compute index3 for each pixel in the latitudinal range for
			% the day.
			fprintf('\t');
			for y_coord = y_start:y_stop
				iteration = y_coord - y_start + 1;

				index3(iteration) = compute_index3_pixel(posn, x_coord, y_coord, log_temp_u, log_temp_pro, ...
					100, 50, search_box_width, search_box_height, nrows, ncols, lat_step, long_step);
% debugging
%				if (mod(iteration, 60) ~= 0)
%					fprintf('.');
%				else
%					fprintf('.\n\t');
%				end

%				fprintf('Finished ycoord %d (iter = %d) at position %d, going to %d\n', y_coord, iteration, ...
%					posn, y_stop);
			end %for

			%figure out how many index3 scores tied......
			[SST_posns(posn).min_val_3, y_index_3] = min(index3);
		
			%  XXX should check to make sure there actually are any valid
			%  index3 scores (ie. not all Inf)
			
			% find all the index3 scores that tied with the lowest one
			all_min_values_3 = find(index3 == index3(y_index_3));

			% we have num_max_values_3 tied index3 scores
			SST_posns(posn).num_min_values_3 = size(all_min_values_3, 1);

			% get min, max and med of tied lats for index3
			SST_posns(posn).latmax_3 = i_to_g(x_coord, y_index_3 + y_start, nrows, ncols, lat_step, long_step);
			SST_posns(posn).latmin_3 = i_to_g(x_coord, max(all_min_values_3) + y_start, nrows, ncols, lat_step, long_step);
			SST_posns(posn).latmed_3 = i_to_g(x_coord, median(all_min_values_3) + y_start, nrows, ncols, lat_step, long_step);

			SST_posns(posn).SST_lat = SST_posns(posn).latmed_3;
			last_good_SST_lat = SST_posns(posn).SST_lat;	% save to anchor next position within delta_lat degrees.
			total_delta_lat = 0;		% reset search limit offset
			fprintf('\tindex3: %f %d tied; latmin: %.2f, latmax: %.2f, latmed: %.2f\n', ...
				SST_posns(posn).min_val_3, SST_posns(posn).num_min_values_3, ...
				SST_posns(posn).latmin_3, SST_posns(posn).latmax_3, SST_posns(posn).latmed_3);    

		end
	end
	
	fprintf('\tchosen SST lat: %.2f\n\n', SST_posns(posn).SST_lat);

	% reset for next posn
	index1 = [];
	index2 = [];
	index3 = [];
end % for

end
