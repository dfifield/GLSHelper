%function dbimportdemo()
%DBIMPORTDEMO Imports data into MATLAB from a database.

%   Version 1.0  21-Oct-1997
%   Author(s): E.F. McGoldrick, 12/5/1997
%   Copyright 1984-2002 The MathWorks, Inc.

% $Revision: 1.9 $   $Date: 2002/06/06 15:04:18 $

% Set maximum time allowed for establishing a connection.

timeoutA=logintimeout(5)

% Connect to a database.

connA=database('SST','','')

% Check the database status.

ping(connA)

% Open cursor and execute SQL statement.

cursorA=exec(connA,'select * from SST_Info');

% Fetch the first 10 rows of data.

cursorA=fetch(cursorA,10)

% Display the data.

AA=cursorA.Data

% Close the cursor and the connection.

close(cursorA)
close(connA)
