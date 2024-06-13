%testing compute_index1_pixel

%[nrows, ncols] = size(SST(1).temp);
%lat_step = SST(1,1).info.Attributes(1,43).Value;
%long_step = SST(1,1).info.Attributes(1,44).Value;


%[bb_x_nw, bb_y_nw, bb_x_se, bb_y_se] = get_bbox(1400, 500, 500, nrows, ncols, lat_step, long_step);
%compute_index1_pixel(500, log_temps, SST(1).temp, bb_x_nw, bb_y_nw, bb_x_se, bb_y_se)
[success] = find_and_read_sst_file(datenum('9-Apr-2006 05:00:00'));


%for i = 1:3365
%temp(1, i) = log_temps(i).temp;
%temp(2, i) = datenum(log_temps(i).date);
%end