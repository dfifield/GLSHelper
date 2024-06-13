function [psum, Npsum] = compute_index1and2_pixel(log_temp, search_area, sec_per_temp, delta)
% compute_index1and2_pixel() - compute the first and second indices of match (Teo et al) for
% a given coordinate pair under consideration.
% log_temp - array of logger temps for the current day. Structure is (date, temp)
% search_area - 2D array of temperatures from sat within the search area
% for this pixel.
% sec_per_temp - number of seconds represented by each temperature (ie. 120
% for GeoLT)
% delta - defines how close temps must be to be considered a match.

% Local vars:
% p[i] - proportion of time spent at the ith matching SST
% Np[i] - p[i] * num_matching pixels

% Sort and unique the logger and satellite temp lists. Necessary to make
% the finds more efficient 
log_temp_sort = sort(unique(log_temp(:,2)));
[num_uniq_log_temp, junk] = size(log_temp_sort);

% XXX check into this. Sort just sorts each column. Is this what I want?
sst_temp_sort = sort(search_area);

% Find all matching temperatures within delta degrees between the logger
% and the search area and save the list of matching logger temps in
% t_match.
k = 0;

% p and Np will require at most num_uniq_log_temp entries
p = zeros(num_uniq_log_temp, 1);
Np = zeros(num_uniq_log_temp, 1);

% look for each unique logger temperature in the list of sst_temps +/-
% delta. Compute index1 and index2 scores for this pixel.
for i=1:num_uniq_log_temp
    % find indices of all elements in sst_temp_sort that
    % match the logger temperature within delta degrees.
    found = find((log_temp_sort(i) - delta <= sst_temp_sort) & (log_temp_sort(i) + delta >= sst_temp_sort));
    
    % if a match is found then add the logger temp to the list of matching
    % temperatures for this pixel and compute p[i].
    if found % true if found is not empty
        k = k + 1;  % k is the count of matching temperatures
        
        % debugging - not actually used
		% t_match(k) = log_temp_sort(i);
		% imagesc(search_area); figure(gcf);
		
        % count the number of times the temperature appears in the logger data on this day
        % and multiply by number fraction of the day that each temperature reading represents
        % ie. 2 mins for GeoLT) to get p[i]. 
        num_occur = size(find(log_temp(:,2) == log_temp_sort(i)), 1);
        p(k) = num_occur * (sec_per_temp/86400);
		
		% compute the second index: Np = p(k) * Num_matching_pixels
		[num_pixel, junk] = size(found);
		Np(k) = p(k) * num_pixel;
	end
end

% sum up all the p[i]'s and Np[i]'s for this pixel and return as result
psum = sum(p);
Npsum = sum(Np);
end