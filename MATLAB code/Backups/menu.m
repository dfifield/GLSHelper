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

% Last Modified by GUIDE v2.5 17-May-2008 09:59:06

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

global options;

% --- Executes just before menu is made visible.
function menu_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to menu (see VARARGIN)

% Choose default command line output for menu
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes menu wait for user response (see UIRESUME)
% uiwait(handles.figure1);


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

[filename, pathname, filterindex] = uigetfile('*.txt', 'Select Geologger Position File');
if (filename ~= 0) 
	set(handles.Position_File, 'String', [pathname, filename]);	
	options.posn_file = [pathname, filename];
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

[filename, pathname, filterindex] = uigetfile('*.txt', 'Select Geologger Temperature File');
if (filename ~= 0) 
	set(handles.Temperature_File, 'String', [pathname, filename]);	
	options.temp_file = [pathname, filename];
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

dirname = uigetdir('', 'Select Folder Containing Satellite SST Data');
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

[filename, pathname, filterindex] = uiputfile('*.txt', 'Select Output File');
if (filename ~= 0) 
	set(handles.Output_File, 'String', [pathname, filename]);	
	options.output_file = [pathname, filename];
end



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

