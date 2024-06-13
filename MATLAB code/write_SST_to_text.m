global SST;

[m, n] = size(SST(1,1).temp);
fid = fopen('temp.txt', 'w');
for i = 1:m 
	for j = 1:n
		fprintf(fid, '%f,', SST(1,1).temp(i,j));
	end
	i
end

fclose(fid);