function varargout = menu(varargin)
% MENU M-file for menu.fig
%      MENU, by itself, creates a new MENU or raises the existing
%      singleton*.
%
%      H = MENU returns the handle to a new MENU or the handle to
%      the existing singleton*.
%
%      MENU('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in MENU.M with the given input arguments.
%
%      MENU('Property','Value',...) creates a new MENU or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before menu_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to menu_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help menu

% Last Modified by GUIDE v2.5 15-Feb-2009 20:23:53

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @menu_OpeningFcn, ...
                   'gui_OutputFcn',  @menu_OutputFcn, ...
                   'gui_LayoutFcn',  [] , ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT




% --- Executes just before menu is made visible.
function menu_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to menu (see VARARGIN)

global options; 

% Choose default command line output for menu
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes menu wait for user response (see UIRESUME)
% uiwait(handles.figure1);


%set visiblity of temp file type buttons as appropriate.
switch options.temp_file_type % Get Tag of selected object.
    case 'BAS'
        Set_BAS_Temp_Type();
    case 'Geoltv1'
        % Code for when Geoltv1 is selected.
        Set_Geoltv1_Temp_Type();
    case 'Geoltv2'
        Set_Geoltv2_Temp_Type();        
    otherwise
        % Code for when there is no match.
        msgbox('Illegal tag in Temperature File Type in menu.m','error', 'modal');
end

% --- Outputs from this function are returned to the command line.
function varargout = menu_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;

function N_Limit_Callback(hObject, eventdata, handles)
% hObject    handle to N_Limit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global options;

% Hints: get(hObject,'String') returns contents of N_Limit as text
options.n_limit = str2double(get(hObject,'String')); % returns contents of N_Limit as a double



% --- Executes during object creation, after setting all properties.
function N_Limit_CreateFcn(hObject, eventdata, handles)
% hObject    handle to N_Limit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

global options;

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', options.n_limit); % populate field from saved options struct.


function S_Limit_Callback(hObject, eventdata, handles)
% hObject    handle to S_Limit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global options;

% Hints: get(hObject,'String') returns contents of S_Limit as text
%        str2double(get(hObject,'String')) returns contents of S_Limit as a double
options.s_limit = str2double(get(hObject,'String')); % returns contents of S_Limit as a double

% --- Executes during object creation, after setting all properties.
function S_Limit_CreateFcn(hObject, eventdata, handles)
% hObject    handle to S_Limit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

global options;

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', options.s_limit); % populate field from saved options struct.



% --- Executes on button press in Go.
function Go_Callback(hObject, eventdata, handles)
% hObject    handle to Go (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
global options;

% Save current option set to SST.m
save('SST', 'options');
main();



function Position_File_Callback(hObject, eventdata, handles)
% hObject    handle to Position_File (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global options;


% Hints: get(hObject,'String') returns contents of Position_File as text
%        str2double(get(hObject,'String')) returns contents of Position_File as a double
options.posn_file = get(hObject,'String'); 

% --- Executes during object creation, after setting all properties.
function Position_File_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Position_File (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

global options;


% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', options.posn_file); % populate field from saved options struct.


% --- Executes on button press in Position_File_Select.
function Position_File_Select_Callback(hObject, eventdata, handles)
% hObject    handle to Position_File_Select (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global options;

% get current contents of box

% save current dir, cd to data folder if it exists, get filename and cd back to saved
% cwd.
savedir = pwd;
if ((options.datafolder ~= 0))
	if ((exist(options.datafolder, 'dir') == 7))
		cd(options.datafolder);
	end
end
[filename, pathname, filterindex] = uigetfile('*.txt', 'Select Geologger Position File');
cd(savedir);

% process user choice
if (filename ~= 0) 
	set(handles.Position_File, 'String', [pathname, filename]);	
	options.posn_file = [pathname, filename];
	
	% if user chose a folder different from options.datafolder, then save new folder as
	% new cwd and cd there
	if (~strcmp(pathname, options.datafolder))
		options.datafolder = pathname;
	end
end



function Temperature_File_Callback(hObject, eventdata, handles)
% hObject    handle to Temperature_File (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global options;


% Hints: get(hObject,'String') returns contents of Temperature_File as text
%        str2double(get(hObject,'String')) returns contents of Temperature_File as a double
options.temp_file = get(hObject,'String'); 

% --- Executes during object creation, after setting all properties.
function Temperature_File_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Temperature_File (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

global options;

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', options.temp_file); % populate field from saved options struct.


% --- Executes on button press in Temperature_File_Select.
function Temperature_File_Select_Callback(hObject, eventdata, handles)
% hObject    handle to Temperature_File_Select (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global options;

% save current dir, cd to data folder, get filename and cd back to saved
% cwd.
savedir = pwd;
if ((options.datafolder ~= 0))
	if ((exist(options.datafolder, 'dir') == 7))
		cd(options.datafolder);
	end
end

[filename, pathname, filterindex] = uigetfile('*.txt', 'Select Logger Temperature File');
cd(savedir);

if (filename ~= 0) 
	set(handles.Temperature_File, 'String', [pathname, filename]);	
	options.temp_file = [pathname, filename];

	% if user chose a folder different from options.datafolder, then save new folder as
	% new cwd and cd there
	if (~strcmp(pathname, options.datafolder))
		options.datafolder = pathname;
	end
end


function Output_File_Callback(hObject, eventdata, handles)
% hObject    handle to Output_File (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global options;

% Hints: get(hObject,'String') returns contents of Output_File as text
%        str2double(get(hObject,'String')) returns contents of Output_File as a double
options.output_file = get(hObject,'String'); 

% --- Executes during object creation, after setting all properties.
function Output_File_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Output_File (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

global options

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', options.output_file);


% --- Executes on button press in Output_File_Select.
function Output_File_Select_Callback(hObject, eventdata, handles)
% hObject    handle to Output_File_Select (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global options;

% save current dir, cd to data folder, get filename and cd back to saved
% cwd.
savedir = pwd;
if ((options.datafolder ~= 0))
	if ((exist(options.datafolder, 'dir') == 7))
		cd(options.datafolder);
	end
end
[filename, pathname, filterindex] = uiputfile('*.txt', 'Select Output File');
cd(savedir);

if (filename ~= 0) 
	set(handles.Output_File, 'String', [pathname, filename]);	
	options.output_file = [pathname, filename];
		
	% if user chose a folder different from options.datafolder, then save new folder as
	% new cwd and cd there
	if (~strcmp(pathname, options.datafolder))
		options.datafolder = pathname;
	end
end


function Search_Box_Width_Callback(hObject, eventdata, handles)
% hObject    handle to Search_Box_Width (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global options;


% Hints: get(hObject,'String') returns contents of Search_Box_Width as text
%        str2double(get(hObject,'String')) returns contents of Search_Box_Width as a double
options.search_width = str2double(get(hObject,'String')); 

% --- Executes during object creation, after setting all properties.
function Search_Box_Width_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Search_Box_Width (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

global options;

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', options.search_width);


function Search_Box_Height_Callback(hObject, eventdata, handles)
% hObject    handle to Search_Box_Height (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global options;


% Hints: get(hObject,'String') returns contents of Search_Box_Height as text
%        str2double(get(hObject,'String')) returns contents of Search_Box_Height as a double
options.search_height = str2double(get(hObject,'String')); 


% --- Executes during object creation, after setting all properties.
function Search_Box_Height_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Search_Box_Height (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

global options;


% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', options.search_height);


function SST_Folder_Callback(hObject, eventdata, handles)
% hObject    handle to SST_Folder (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global options;

options.SST_folder = get(hObject,'String'); 

% Hints: get(hObject,'String') returns contents of SST_Folder as text
%        str2double(get(hObject,'String')) returns contents of SST_Folder as a double


% --- Executes during object creation, after setting all properties.
function SST_Folder_CreateFcn(hObject, eventdata, handles)
% hObject    handle to SST_Folder (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

global options;


% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', options.SST_folder);

% --- Executes on button press in SST_Folder_Select.
function SST_Folder_Select_Callback(hObject, eventdata, handles)
% hObject    handle to SST_Folder_Select (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global options;

savedir = pwd;
if ((options.SST_folder ~= 0))
	if ((exist(options.SST_folder, 'dir') == 7))
		cd(options.SST_folder);
	end
end
dirname = uigetdir('', 'Select Folder Containing Satellite SST Data');
cd (savedir)

if (dirname ~= 0) 
	set(handles.SST_Folder, 'String', dirname);	
	options.SST_folder = dirname;
end



function Delta_T_Callback(hObject, eventdata, handles)
% hObject    handle to Delta_T (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global options;


% Hints: get(hObject,'String') returns contents of Delta_T as text
%        str2double(get(hObject,'String')) returns contents of Delta_T as a double
options.delta_t = str2double(get(hObject,'String')); 

% --- Executes during object creation, after setting all properties.
function Delta_T_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Delta_T (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

global options;


% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', options.delta_t);




function Secs_Per_Temp_Callback(hObject, eventdata, handles)
% hObject    handle to Secs_Per_Temp (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global options;

% Hints: get(hObject,'String') returns contents of Secs_Per_Temp as text
%        str2double(get(hObject,'String')) returns contents of Secs_Per_Temp as a double
options.secs_per_temp = str2double(get(hObject,'String')); 

% --- Executes during object creation, after setting all properties.
function Secs_Per_Temp_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Secs_Per_Temp (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

global options;

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', options.secs_per_temp);



function Delta_Lat_Callback(hObject, eventdata, handles)
% hObject    handle to Delta_Lat (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

global options;

% Hints: get(hObject,'String') returns contents of Delta_Lat as text
%        str2double(get(hObject,'String')) returns contents of Delta_Lat as a double
options.delta_lat = str2double(get(hObject,'String')); 

% --- Executes during object creation, after setting all properties.
function Delta_Lat_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Delta_Lat (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

global options;

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', options.delta_lat);


function StartLat_Callback(hObject, eventdata, handles)
% hObject    handle to StartLat (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
global options;
% Hints: get(hObject,'String') returns contents of StartLat as text
%        str2double(get(hObject,'String')) returns contents of StartLat as a double
options.start_lat = str2double(get(hObject,'String')); 

% --- Executes during object creation, after setting all properties.
function StartLat_CreateFcn(hObject, eventdata, handles)
% hObject    handle to StartLat (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
global options;
% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', options.start_lat);


function StartLong_Callback(hObject, eventdata, handles)
% hObject    handle to StartLong (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
global options;
% Hints: get(hObject,'String') returns contents of StartLong as text
%        str2double(get(hObject,'String')) returns contents of StartLong as a double
options.start_long = str2double(get(hObject,'String')); 

% --- Executes during object creation, after setting all properties.
function StartLong_CreateFcn(hObject, eventdata, handles)
% hObject    handle to StartLong (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
global options;
% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', options.start_long);



% --- Executes during object creation, after setting all properties.
function LogStartText_CreateFcn(hObject, eventdata, handles)
% hObject    handle to LogStartText (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
global options;

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', datestr(options.log_start));


% --- Executes on button press in DateButton.
function DateButton_Callback(hObject, eventdata, handles)
% hObject    handle to DateButton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
global options;

% set the date/time in options and display it.
if (isempty(options.log_start))
	t = uigetdate();
else
	t = uigetdate(options.log_start);
end

options.log_start = t;
h = findobj('Tag', 'LogStartText');
set(h, 'String', datestr(options.log_start));



function temp_interval_Callback(hObject, eventdata, handles)
% hObject    handle to temp_interval (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of temp_interval as text
%        str2double(get(hObject,'String')) returns contents of temp_interval as a double
global options;

options.temp_interval = str2double(get(hObject,'String'));

% --- Executes during object creation, after setting all properties.
function temp_interval_CreateFcn(hObject, eventdata, handles)
% hObject    handle to temp_interval (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

global options;

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', options.temp_interval);



function Delta_T_Extract_Callback(hObject, eventdata, handles)
% hObject    handle to Delta_T_Extract (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of Delta_T_Extract as text
%        str2double(get(hObject,'String')) returns contents of Delta_T_Extract as a double
global options;

options.delta_t_extract = str2double(get(hObject,'String'));


% --- Executes during object creation, after setting all properties.
function Delta_T_Extract_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Delta_T_Extract (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

global options;

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', options.delta_t_extract);



function Min_Surface_Time_Callback(hObject, eventdata, handles)
% hObject    handle to Min_Surface_Time (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of Min_Surface_Time as text
%        str2double(get(hObject,'String')) returns contents of Min_Surface_Time as a double
global options;

options.min_surface_time = str2double(get(hObject,'String'));


% --- Executes during object creation, after setting all properties.
function Min_Surface_Time_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Min_Surface_Time (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

global options;

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
set(hObject, 'String', options.min_surface_time);

% --- Executes during object creation, after setting all properties.
function BAS_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BAS (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
global options;

% Start out with button turned off
set(hObject, 'Value', get(hObject, 'Min'));

% --- Executes on button press in BAS.
function BAS_Callback(hObject, eventdata, handles)
% hObject    handle to BAS (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
global options;

% Hint: get(hObject,'Value') returns toggle state of BAS
set(hObject,'Value', get(hObject, 'Max'));
Set_BAS_Temp_Type()


function Set_BAS_Temp_Type()

global options;

    options.temp_file_type = 'BAS';

    % Toggle BASon
    h = findobj('Tag', 'BAS');
    set(h, 'Value', get(h, 'Max'));

    % Toggle Geoltv1 and Geoltv2 off
    h = findobj('Tag', 'Geoltv1');
    set(h, 'Value', get(h, 'Min'));
    h = findobj('Tag', 'Geoltv2');
    set(h, 'Value', get(h, 'Min'));
    
    % Turn off logger start controls since not needed for BAS
    h = findobj('Tag', 'LogStartLabel');
    set(h, 'Enable', 'Off');
    h = findobj('Tag', 'LogStartText');
    set(h, 'Enable', 'Off');
    h = findobj('Tag', 'DateButton');
    set(h, 'Enable', 'Off');
    h = findobj('Tag', 'Secs_Per_Temp');
    set(h, 'Enable', 'Off');
    h = findobj('Tag', 'Secs_Per_Temp_Label');
    set(h, 'Enable', 'Off');

    % Turn off surface temp extraction controls since BAS loggers only
    % record surface temps anyway so no need to extract.
    h = findobj('Tag', 'Logger_Surface_Label');
    set(h, 'Enable', 'Off');
    h = findobj('Tag', 'Logger_Temp_Equiv_Label');
    set(h, 'Enable', 'Off');
    h = findobj('Tag', 'Min_Surface_Time_Label');
    set(h, 'Enable', 'Off');
    h = findobj('Tag', 'Delta_T_Extract');
    set(h, 'Enable', 'Off');
    h = findobj('Tag', 'Min_Surface_Time');
    set(h, 'Enable', 'Off');

    % These are not necessary for BAS loggers
    options.log_start = [];
    options.delta_t_extract = [];
    options.min_surface_time = [];



% --- Executes during object creation, after setting all properties.
function Geoltv1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Geoltv1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Start out with button turned off
set(hObject, 'Value', get(hObject, 'Min'));

% --- Executes on button press in Geoltv1.
function Geoltv1_Callback(hObject, eventdata, handles)
% hObject    handle to Geoltv1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of Geoltv1

set(hObject,'Value', get(hObject, 'Max'));
Set_Geoltv1_Temp_Type()

function Set_Geoltv1_Temp_Type()
global options;
    options.temp_file_type = 'Geoltv1';

    % Toggle Geoltv1 on
    h = findobj('Tag', 'Geoltv1');
    set(h, 'Value', get(h, 'Max'));

    % Toggle BAS and Geoltv2 off
    h = findobj('Tag', 'BAS');
    set(h, 'Value', get(h, 'Min'));
    h = findobj('Tag', 'Geoltv2');
    set(h, 'Value', get(h, 'Min'));

    % Turn on logger start time control since temp file doesn't have dates
    % or times.
    h = findobj('Tag', 'LogStartLabel');
    set(h, 'Enable', 'On');
    h = findobj('Tag', 'LogStartText');
    set(h, 'Enable', 'On');
    h = findobj('Tag', 'DateButton');
    set(h, 'Enable', 'On');
    h = findobj('Tag', 'Secs_Per_Temp');
    set(h, 'Enable', 'On');
    h = findobj('Tag', 'Secs_Per_Temp_Label');
    set(h, 'Enable', 'On');
    
    % Turn on surface temperature extraction controls
    h = findobj('Tag', 'Logger_Surface_Label');
    set(h, 'Enable', 'On');
    h = findobj('Tag', 'Logger_Temp_Equiv_Label');
    set(h, 'Enable', 'On');
    h = findobj('Tag', 'Min_Surface_Time_Label');
    set(h, 'Enable', 'On');
    h = findobj('Tag', 'Delta_T_Extract');
    set(h, 'Enable', 'On');
    h = findobj('Tag', 'Min_Surface_Time');
    set(h, 'Enable', 'On');
    
    
% --- Executes during object creation, after setting all properties.
function Geoltv2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Geoltv2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Start out with button turned off
set(hObject, 'Value', get(hObject, 'Min'));


% --- Executes on button press in Geoltv2.
function Geoltv2_Callback(hObject, eventdata, handles)
% hObject    handle to Geoltv2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of Geoltv2

set(hObject,'Value', get(hObject, 'Max'));
Set_Geoltv2_Temp_Type()

   
function Set_Geoltv2_Temp_Type()
global options;
    options.temp_file_type = 'Geoltv2';

    % Toggle Geoltv2 on
    h = findobj('Tag', 'Geoltv2');
    set(h, 'Value', get(h, 'Max'));
    
    % Toggle Geoltv1 and BAS off
    h = findobj('Tag', 'Geoltv1');
    set(h, 'Value', get(h, 'Min'));
    h = findobj('Tag', 'BAS');
    set(h, 'Value', get(h, 'Min'));


    % Turn off logger start time control since temp file has dates
    % and times.
    h = findobj('Tag', 'LogStartLabel');
    set(h, 'Enable', 'Off');
    h = findobj('Tag', 'LogStartText');
    set(h, 'Enable', 'Off');
    h = findobj('Tag', 'DateButton');
    set(h, 'Enable', 'Off');
    h = findobj('Tag', 'Secs_Per_Temp');
    set(h, 'Enable', 'Off');
    h = findobj('Tag', 'Secs_Per_Temp_Label');
    set(h, 'Enable', 'Off');
    
    % Turn on surface temperature extraction controls
    h = findobj('Tag', 'Logger_Surface_Label');
    set(h, 'Enable', 'On');
    h = findobj('Tag', 'Logger_Temp_Equiv_Label');
    set(h, 'Enable', 'On');
    h = findobj('Tag', 'Min_Surface_Time_Label');
    set(h, 'Enable', 'On');
    h = findobj('Tag', 'Delta_T_Extract');
    set(h, 'Enable', 'On');
    h = findobj('Tag', 'Min_Surface_Time');
    set(h, 'Enable', 'On');


% --- Executes when user attempts to close figure1.
function figure1_CloseRequestFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
global options;

selection = questdlg('Close SST Correction?', 'Close','Yes','No','Yes'); 
switch selection
    case 'Yes'
    save('SST', 'options');
    delete(hObject);
case 'No'      
    return 
end
 


% --- Executes on button press in pushbutton7.
function pushbutton7_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
h = findobj('Tag', 'figure1');
delete(h);

