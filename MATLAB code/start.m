% global vars
global SST;          % temperature data from satellite
global SST_headers;  % Header info for all available SST datafiles
global SST_posns;	 % the output data structure filled in by this code
global options;
global log_posn;
global log_temp;	 % logger surface temperatures
global log_temp_all;	% all logger temps before surface extraction

load -mat SST
menu();

