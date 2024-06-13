% re-zip SST data files

cd('D:\SST');
files = dir('D:\SST');
[numfiles] = size(files,1);

for i = 1:numfiles
	if (files(i).isdir == false) % ignore directories (ie. . and ..)
        files(i).name % debugging output
		system(['..\bzip2 ', files(i).name]);
	end
end
