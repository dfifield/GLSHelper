GLSHelper
=======
Quickly process multiple GLS datasets

This package allows you to process many GLS tag datasets quickly
    with a single function call using the approach laid out in the
    [Light Level Geolocation Analysis](https://geolocationmanual.vogelwarte.ch/) manual.
    Internally it uses the GeoLight
    package to perform the computations. It uses a configuration file (CSV format) to
    specify options for each tag dataset. It includes options for selecting light
    thresholds, removing equinoxes, date filtering, speed filtering, outlier removal,
    sliding window smoothing, 
    creating any number of kernel UD contours, writing points and kernel UDs to
    shapefiles, ploting activity (wet/dry) data, and producing a leaflet map. An example 
    dataset, configuration file, and analysis script are included.
    
Installation
=======

    if(!require(devtools)) install.packages("devtools")
    devtools::install_github("dfifield/GLSHelper")
