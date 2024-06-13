global db;

conn=database('SST','','');

% Null numbers should be read as zeros
setdbprefs('NullNumberRead','0')

% Check status
ping(conn)

% Open a cursor and execute a fetch.
%infocurs=exec(conn,'select * from SST_Info');
%infocurs=fetch(infocurs);

[junk, numSSTfiles] = size(SST);

for i = 1:1
	xc{1} = SST(1,i).info.Attributes(1,1).Value;
	xc{2} = SST(1,i).info.Attributes(1,12).Value;
	xc{3} = SST(1,i).info.Attributes(1,21).Value;
	xc{4} = SST(1,i).info.Attributes(1,22).Value;
	xc{5} = SST(1,i).info.Attributes(1,23).Value;
	xc{6} = SST(1,i).info.Attributes(1,24).Value;
	xc{7} = SST(1,i).info.Attributes(1,43).Value;
	xc{8}  = SST(1,i).info.Attributes(1,44).Value;
	xc{9} = SST(1,i).info.Attributes(1,45).Value;
	xc{10} = SST(1,i).info.Attributes(1,46).Value;
	xc{11} = SST(1,i).info.Attributes(1,48).Value;
	xc{12} = SST(1,i).info.Attributes(1,49).Value;
	xc{13} = SST(1,i).info.Attributes(1,53).Value;
	xc{14} = SST(1,i).info.Attributes(1,54).Value;
	xc{15} = SST(1,i).info.Attributes(1,55).Value;
	xc{16} = SST(1,i).info.Attributes(1,56).Value;
	xc{17} = SST(1,i).info.Attributes(1,57).Value;
	xc{18} = SST(1,i).info.Attributes(1,58).Value;
	xc{19} = SST(1,i).info.Attributes(1,59).Value;
	xc{20} = SST(1,i).info.Attributes(1,60).Value;
    
    coln = {'ProductName','ProductType','StartYear','StartDay','EndYear','EndDay','SWLat','SWLong','LatStep','LongStep','NLines','NCols','Scaling','ScaleEqn','Slope','Intercept','ScaledDataMin','ScaledDataMax','DataMin','DataMax'};
    
    insert(conn, 'SST_Info', coln, xc);
     
    % re-Open =a cursor and execute a fetch to get the most recently added
    % record and it's ID
	infocurs=exec(conn,'SELECT Max(SST_Info.ID) AS MaxOfID FROM SST_Info');
%	infocurs=exec(conn,'SELECT * FROM SST_Info');
    infocurs=fetch(infocurs);
    
    % Get ID of last record added
    [n,m] = size(infocurs.data);
    parent = infocurs.data{n,m}
    rows = double(xc{11});
    cols = double(xc{12});
%    rows = infocurs.data{n,13};
%    cols = infocurs.data{n,14};

    % insert temperature data
    coln = {'ParentID', 'X', 'Y', 'Val'};
	for r = 1:rows
        r
		for c = 1:cols
			xd{c, 1} = parent;
			xd{c, 2} = r;
			xd{c, 3} = c;
            xd{c, 4} = SST(1,1).temp(r,c);
        end
        insert(conn, 'SST_Data', coln, xd);    
    end
end
close(conn);
