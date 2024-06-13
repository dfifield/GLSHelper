

randn('state', sum(100*clock));
center_x is x coord or center of search area - param passed in
center_y is y coord or center of search area - param passed in


% for each generated offset find the pixel coordinates
for i = 1:100
	
	% draw 50 random samples from the search area with bivariate normal
	% distribution around center.
	x=normrnd(0, search_box_width/4, 50, 1);
	y=normrnd(0, search_box_height/4, 50, 1);

	for j = 1:50
		% get coordinates of a box that is offset by x,y km from the
		% center. Use get_bbox since it is already written and provides
		% what we want.
		[bb_x_nw, bb_y_nw, bb_x_se, bb_y_se] = get_bbox(center_x, center_y, abs(x(j)) * 2, abs(y(j)) * 2, nrows, ncols, lat_step, long_step);
		
		% Figure out which corner of the bbox is the right one based on the
		% sign of the offsets.
		
		
		% get temperature at that location and save it in sample of 50

		[lat, long] = i_to_g(a(1,1), a(1,2), nrows, ncols, lat_step, long_step);
	end
end

scatter(a(:,1), a(:,2)); figure(gcf);