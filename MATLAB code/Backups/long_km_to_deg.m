function deg = long_km_to_deg(km, lat)
% long_km_to_deg - Return the number of degrees longitude equivalent to km
% km at given latitude lat.

deg = (km/111.12)/cosd(lat);
end