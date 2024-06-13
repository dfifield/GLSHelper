function d = day_to_date(day, year)
% day_to_date(day, year) - convert a day of the year (eg. 233rd day) to an actual date
% d - returned value is a datenum.
%
	d = addtodate(datenum([year, 1, 1, 0, 0, 0]), day - 1, 'day');
end