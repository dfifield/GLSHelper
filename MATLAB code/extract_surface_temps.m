function surface_temps = extract_surface_temps(raw_temps, delta_t, min_time)
% extract_surface_temps(raw_temps, delta_t, min_time)
% extract all logger temperatures from raw_temps where temp has remained within
% options.delta_t for at least min_time minutes.
%
global options;

% trail_idx - trailing index into start of constant temperature period
% lead_idx - leading index into end of constant temperature period

%initialize
trail_idx = 1;
lead_idx = 2;
result_idx = 1;
length = 0; % length of current period of equiv temps
max_len = 0; % maximum constant temp period found
num_periods = 0; % number of surface periods found
num_found = 0; % number of temps in a given constant temp period
total_found = 0; % total number of surface temps found

min_per_temp = options.secs_per_temp /60; % minutes per temperature reading

temp_size = size(raw_temps,1);

%figure out sizing of results matrices....try creating temporary matrix
% of full size and then eventually dimensioning the final result for the
% number of surface temps found.
result = zeros(temp_size, 2);

% Process all temperature readings in raw_temps
while(lead_idx <= temp_size && trail_idx <= temp_size)
	
	% find extent of current period of constant temp as long as we don't
	% run out of temps.
	diff = abs(raw_temps(lead_idx,2) - raw_temps(trail_idx, 2));
	while (lead_idx <= temp_size &&  diff <= delta_t)
		lead_idx = lead_idx + 1;

		% 	move abs(....) back into while conditional above and this if
		% 	statement should be unneccessary.
		if (lead_idx <= temp_size)
			diff = abs(raw_temps(lead_idx,2) - raw_temps(trail_idx, 2));
		end
	end
	
	%lead_idx now points to start of next potential period or is greater
	%than temp_size (ie. we're done)
	
	% get length of period of constant temp, add to total and compute
	% length in minutes of this curren period
	num_found = lead_idx - trail_idx;
	length = num_found * min_per_temp;
	
	% found a period of sufficient length to be considered on water
	if (length > min_time)
		num_periods = num_periods + 1;
		
		% copy surface temps to temporary result
		result(result_idx:result_idx + num_found - 1, :) = raw_temps(trail_idx:lead_idx - 1, :);
		result_idx = result_idx + num_found;
		
		total_found = total_found + num_found;
		
		% move trailing pointer to past the end of this period to begin
		% next time. XXX this needs to be fixed?? since it will fail in a 
		% situattion of 30mins on water where first temp is slightly higher and with a slight 
		% drop in temp after say 21 mins that is to far from value of first
		% temp. This algorithm would give one period of 21 mins on water
		% and then a period of 9 mins where it should have been on water
		% but won't be. This is because the statement below does not allow
		% "overlapping" periods on water. Not sure how to fix. could just
		% move trail_idx forward by one each time and allow overlapping
		% periods, but this might lead to marking a whole period of flight
		% where temperature creeps up slowly as water. Is there a way to
		% extract maximal length on water section...??
		% but it is how i used to do
		% it in access.
		trail_idx = lead_idx;
	else
		% Period too short. Restart the search at second record from start 
		% of this too short period
		trail_idx = trail_idx + 1;
	end
	
	% reinitialize and go around again
	lead_idx = trail_idx + 1;
	length = 0;
end

surface_temps = zeros(total_found, 2);
surface_temps(1:total_found, :) = result(1:total_found, :);
result = [];