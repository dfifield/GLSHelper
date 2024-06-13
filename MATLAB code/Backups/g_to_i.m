function [x,y] = g_to_i(lat, long, nrows, ncols, lat_step, long_step)
%Converts geographic coordinates in the form of latitude and longitude into
%indices into the temperature array.
% x,y = (1,1) is 90N, 180W

if (lat < -90.0) || (lat > 90.0)
	error('Latitude must be between -90 & 90');
end

if (long < -180.0) || (long > 180.0)
	error('Longitude must be between -180 & 180');
end

y = round(((90.0 - lat)/lat_step) + 1);
if y > nrows
	y = nrows;
end

x = round(((long + 180)/long_step) + 1);
if x > ncols
	x = ncols;
end
end
