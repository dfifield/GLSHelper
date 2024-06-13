function [x_nw, y_nw, x_se, y_se] = get_bbox(x, y, width, height, nrows, ncols, lat_step, long_step)
%get_bbox(x, y, width, height, nrows, ncols, lat_step, long_step) - Return the temp matrix coordinates 
% of a width x height(km) bounding box centered on wpixel at x, y. 
% x,y - center of search box
% width - size of search box in x (east-west) direction
% height- size of search box in y (north-south) direction
%nrows, ncols - Size of the temperature matrix
%lat_step, long_step -  incremental step in latitude or longitude between
%rows and columns of the temperature matrix

% Get latitude and longitude of center of bounding box
[lat, long] = i_to_g(x, y, nrows, ncols, lat_step, long_step);

%Figure out y coordinates
lat_offset = lat_km_to_deg(height/2);
n_lat = lat + lat_offset;
s_lat = lat - lat_offset;

% test and deal with bounding boxes that are too far north or south
if (n_lat > 90.0)
    n_lat = 90.0;
end

if (s_lat < -90.0)
    s_lat = 90.0;
end

% Figure out x coordinates

% Get the number of degrees of longitude equivalent to d/2 kilometers at
% this latitude.
long_offset = long_km_to_deg(width/2, lat);
w_long = long - long_offset;
e_long = long + long_offset;

% handle roll over at 180 degrees long
if w_long < -180
	w_long = w_long + 360;
end

if e_long > 180
	e_long = e_long - 360;
end

%  convert geographic coordinates to indices
% should probably tell g_to_i which way to round it's answer?
[x_nw, y_nw] = g_to_i(n_lat, w_long, nrows, ncols, lat_step, long_step);
[x_se, y_se] = g_to_i(s_lat, e_long, nrows, ncols, lat_step, long_step);

end


