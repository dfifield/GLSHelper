## Version 0.1.3

------------------------------------------------------------------------

-   Removed dependence on GeoLight::twilightCalc() for getting calibration period twilights because it has been deprecated as of GeoLight version 2.1.0. Thanks to Nathanya Goudreau for spotting this. All twilights are now calculated via TwGeos::preprocessLight() (the deployment period twilights were already using this method in version 0.1.2). This led to the deprecation of 3 config file columns: maxLightInt, calibLThresh, and calibAsk.

-   Modified lThresh config parameter to allow non-integer values.

-   Clarify documentation on log transformation of light values in do_multi_gelocation() manual.

-   Clarify package docs to indicate that the analysis follows the approach laid out in [Light Level Geolocation Analysis](https://geolocationmanual.vogelwarte.ch/) manual.

## Version 0.1.2

------------------------------------------------------------------------

-   handle differences between GeoLight 2.0.0 and \>= 2.0.1 more elegantly

## Version 0.1.1

------------------------------------------------------------------------
