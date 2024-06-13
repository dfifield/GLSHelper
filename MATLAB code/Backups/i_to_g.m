function [lat, long] = i_to_g(x, y, nrows, ncols, lat_step, long_step)
% x,y - indices into temperature array to be converted
% SST_Data - used to get the start point and latitude and longitude step
% values.
%
% x,y = (1,1) is 90N, 180W

if (x < 1) || (x > ncols)
	[str, errmsg] = sprintf('Illegal value for x (%d). It must be between 1 and %d.', x, ncols);
	error (str);
end

if (y < 1) || (y > nrows)
	[str, errmsg] = sprintf('Illegal value for y (%d). It must be between 1 and %d.', y, nrows);
	error (str);
end

lat = 90.0 - ((y - 1) * lat_step);
long = -180.0 + ((x - 1) * long_step);

end