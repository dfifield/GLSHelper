%function dbinsert2demo()
%DBINSERT2DEMO Inserts rows into a database table.

%   Version 1.0  21-Oct-1997
%   Author(s): E.F. McGoldrick, 12/5/1997
%   Copyright 1984-2002 The MathWorks, Inc.

% $Revision: 1.12 $   $Date: 2002/06/06 15:04:19 $

% Open a connection.

conn=database('SST','','');

% Null numbers should be read as zeros
setdbprefs('NullNumberRead','0')

% Open a cursor and execute a fetch.

curs=exec(conn,'select * from SST_Info');
curs=fetch(curs);

% Get size of cell array

[m,n]=size(curs.Data);

% Calculate monthly totals for the product entries.


% insert the data.

% insert(conn,'yearlySales',colNames,monthlyTotals);

% Close the cursor and the connection.

close(curs);
close(conn);
