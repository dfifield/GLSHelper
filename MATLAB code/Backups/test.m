%testing compute_index1_pixel
global t_match;

[nrows, ncols] = size(SST(1).temp);
lat_step = SST(1,1).info.Attributes(1,43).Value;
long_step = SST(1,1).info.Attributes(1,44).Value;

xcoord = 1400;
ycoord = 500;
search_box_size = 500;
sec_per_temp = 120;
delta_temp = 0.05

% extract the logger temperatures for the current day and only pass these
% to compute_index1_pixel. Compute datenum of 12:01 midnite (start of day)
% and 12:00 midnite (24hrs later) (end of day) and then use find to get all
% logger temps with datenums between the two. Write a function to do this.
date1 = datenum(2005, 10, 21, 0, 0,0);
date2 = datenum(2005, 10, 22, 0, 0,0);

% find range of indices for day of interest
range = find(log_temps(:,1) >= date1 & log_temps(:,1) < date2);

%extract the logger temp data for the date of interest
[rangesize, junk] = size(range);
day_temp = log_temps(range(1):range(rangesize),:);

% index1 is a vector of index1 scores for each pixel from the n. to s.
% limit for this day.

% index1 = zeros(n. limit - s. limit + 1);

% for ycoord = northern limit to southern limit
    index1(ycoord) = compute_index1_pixel(xcoord, ycoord, search_box_size, day_temp, sec_per_temp, ...
        SST(1).temp, delta_temp, nrows, ncols, lat_step, long_step)

    index1

%for i = 1:3365
%temp(1, i) = log_temps(i).temp;
%temp(2, i) = datenum(log_temps(i).date);
%end