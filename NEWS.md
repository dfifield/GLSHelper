## Version 0.1.3

------------------------------------------------------------------------

-   remove dependence on GeoLight::twilightCalc() for getting calibration period twilights. All twilights are now calculated via TwGeos::preprocessLight() (the deployment period twilights were already using this method in version 0.1.2). This lead to the deprecation of 3 config file columns: maxLightInt, calibLThresh, and calibAsk.

-   modified lThresh config parameter to allow non-integer values.

-   clarify documentation on log transformation of light values in do_multi_gelocation() manual.

-   clarify package docs to indicate that the analysis follows the approach laid out in [Light Level Geolocation Analysis](https://geolocationmanual.vogelwarte.ch/) manual.

## Version 0.1.2

------------------------------------------------------------------------

-   handle differences between GeoLight 2.0.0 and \>= 2.0.1 more elegantly

## Version 0.1.1

------------------------------------------------------------------------
