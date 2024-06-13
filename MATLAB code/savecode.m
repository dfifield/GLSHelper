global db;

db = dbase('SST', '','');
if (isempty(db))
	error('Failed to open SST database');
end

rsSST_Info = rset(db, 'SELECT * from SST_Info');
%fsSST_Info = fields(rsSST_Info);

%fsSST_Info.ProductName = SST(1,1).info.Attributes(1,1).Value;
%fsSST_Info.ProductType = SST(1,1).info.Attributes(1,12).Value;
%fsSST_Info.StartYear = SST(1,1).info.Attributes(1,21).Value;
%fsSST_Info.StartDay = SST(1,1).info.Attributes(1,22).Value;
%%fsSST_Info.StartDate = SST(1,1).info.Attributes(1,12);
%fsSST_Info.EndYear = SST(1,1).info.Attributes(1,23).Value;
%fsSST_Info.EndDay = SST(1,1).info.Attributes(1,24).Value;
%%fsSST_Info.EndDate = SST(1,1).info.Attributes(1,12);
%fsSST_Info.LatStep = SST(1,1).info.Attributes(1,43).Value;
%fsSST_Info.LongStep = SST(1,1).info.Attributes(1,44).Value;
%fsSST_Info.SWLat = SST(1,1).info.Attributes(1,45).Value;
%fsSST_Info.SWLong = SST(1,1).info.Attributes(1,46).Value;
%fsSST_Info.NLines = SST(1,1).info.Attributes(1,48).Value;
%fsSST_Info.NCols = SST(1,1).info.Attributes(1,49).Value;
%fsSST_Info.Scaling = SST(1,1).info.Attributes(1,53).Value;
%fsSST_Info.ScaleEqn = SST(1,1).info.Attributes(1,54).Value;
%fsSST_Info.Slope = SST(1,1).info.Attributes(1,55).Value;
%fsSST_Info.Intercept = SST(1,1).info.Attributes(1,56).Value;
%fsSST_Info.ScaledDataMin = SST(1,1).info.Attributes(1,57).Value;
%fsSST_Info.ScaledDataMax = SST(1,1).info.Attributes(1,58).Value;
%fsSST_Info.DataMin = SST(1,1).info.Attributes(1,59).Value;
%fsSST_Info.DataMax = SST(1,1).info.Attributes(1,60).Value;


xc = fieldc(rsSST_Info);
xc{2} = SST(1,1).info.Attributes(1,1).Value;
xc{3} = SST(1,1).info.Attributes(1,12).Value;
xc{4} = SST(1,1).info.Attributes(1,21).Value;
xc{5} = SST(1,1).info.Attributes(1,22).Value;
%xc{7} = SST(1,1).info.Attributes(1,12);
xc{7} = SST(1,1).info.Attributes(1,23).Value;
xc{8} = SST(1,1).info.Attributes(1,24).Value;
%xc{9} = SST(1,1).info.Attributes(1,12);
xc{10} = SST(1,1).info.Attributes(1,43).Value;
xc{11}  = SST(1,1).info.Attributes(1,44).Value;
xc{12} = SST(1,1).info.Attributes(1,45).Value;
xc{13} = SST(1,1).info.Attributes(1,46).Value;
xc{14} = SST(1,1).info.Attributes(1,48).Value;
xc{15} = SST(1,1).info.Attributes(1,49).Value;
xc{16} = SST(1,1).info.Attributes(1,53).Value;
xc{17} = SST(1,1).info.Attributes(1,54).Value;
xc{18} = SST(1,1).info.Attributes(1,55).Value;
xc{19} = SST(1,1).info.Attributes(1,56).Value;
xc{20} = SST(1,1).info.Attributes(1,57).Value;
xc{21} = SST(1,1).info.Attributes(1,58).Value;
xc{22} = SST(1,1).info.Attributes(1,59).Value;
xc{23} = SST(1,1).info.Attributes(1,60).Value;

xc{2} = SST(1,1).info.Attributes(1,1).Value;
xc{3} = SST(1,1).info.Attributes(1,12).Value;
xc{4} = SST(1,1).info.Attributes(1,21).Value;
xc{5} = SST(1,1).info.Attributes(1,22).Value;
%xc{7} = SST(1,1).info.Attributes(1,12);
xc{7} = SST(1,1).info.Attributes(1,23).Value;
xc{8} = SST(1,1).info.Attributes(1,24).Value;
%xc{9} = SST(1,1).info.Attributes(1,12);
xc{10} = SST(1,1).info.Attributes(1,43).Value;
xc{11}  = SST(1,1).info.Attributes(1,44).Value;
xc{12} = SST(1,1).info.Attributes(1,45).Value;
xc{13} = SST(1,1).info.Attributes(1,46).Value;
xc{14} = SST(1,1).info.Attributes(1,48).Value;
xc{15} = SST(1,1).info.Attributes(1,49).Value;
xc{16} = SST(1,1).info.Attributes(1,53).Value;
xc{17} = SST(1,1).info.Attributes(1,54).Value;
xc{18} = SST(1,1).info.Attributes(1,55).Value;
xc{19} = SST(1,1).info.Attributes(1,56).Value;
xc{20} = SST(1,1).info.Attributes(1,57).Value;
xc{21} = SST(1,1).info.Attributes(1,58).Value;
xc{22} = SST(1,1).info.Attributes(1,59).Value;
xc{23} = SST(1,1).info.Attributes(1,60).Value;


insert(rsSST_Info, xc)


%close(rsSST_Info);
%close(db);
xc{1} = SST(1,1).info.Attributes(1,1).Value;
xc{2} = SST(1,1).info.Attributes(1,12).Value;
xc{3} = int32(SST(1,1).info.Attributes(1,21).Value);
xc{4} = int32(SST(1,1).info.Attributes(1,22).Value);
%xc57} = SST(1,1).info.Attributes(1,12);
xc{6} = SST(1,1).info.Attributes(1,23).Value;
xc{7} = SST(1,1).info.Attributes(1,24).Value;
%xc{8} = SST(1,1).info.Attributes(1,12);
xc{9} = SST(1,1).info.Attributes(1,43).Value;
xc{10}  = SST(1,1).info.Attributes(1,44).Value;
xc{11} = SST(1,1).info.Attributes(1,45).Value;
xc{12} = SST(1,1).info.Attributes(1,46).Value;
xc{13} = SST(1,1).info.Attributes(1,48).Value;
xc{14} = SST(1,1).info.Attributes(1,49).Value;
xc{15} = SST(1,1).info.Attributes(1,53).Value;
xc{16} = SST(1,1).info.Attributes(1,54).Value;
xc{17} = SST(1,1).info.Attributes(1,55).Value;
xc{18} = SST(1,1).info.Attributes(1,56).Value;
xc{19} = SST(1,1).info.Attributes(1,57).Value;
xc{20} = SST(1,1).info.Attributes(1,58).Value;
xc{21} = SST(1,1).info.Attributes(1,59).Value;
xc{22} = SST(1,1).info.Attributes(1,60).Value;

