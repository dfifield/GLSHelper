global db;

conn=database('SST','','');

% Null numbers should be read as zeros
setdbprefs('NullNumberRead','0')

% Check status
ping(conn)
   
% insert temperature data
datacurs=exec(conn,'select * from SST_Data');
datacurs=fetch(datacurs);
coln = {'ParentID', 'X', 'Y', 'Val'};

xd{1} = 123;
xd{2} = 456;
xd{3} = 789;
xd{4} = 1234;
insert(conn, 'SST_DATA', coln, xd);
close(conn);
