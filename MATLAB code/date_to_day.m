function day = date_to_day(date)
% date_to_day(date) - convert a date (datenum) to the number of days from
% start of year.
% day - returned value is an integer representing the number of days that date is from the start of the year.
%
	[y, m, d, h, mn, sec] = datevec(date);
	first_day = datenum(y, 1, 1);	% getdatenum for Jan 1 of this year
	day = date - first_day + 1;
end