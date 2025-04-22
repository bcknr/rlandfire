# rlandfire 2.0.0
-   Added support for the new "version 2" LANDFIRE Products Service (LFPS) API
-   Deprecated the old `landfireAPI()` function in favor of `landfireAPIv2()`
-   Added new features to `landfireAPIv2()` including:
    -   `edit_mask` to restrict edit rules to a specific area using a shapefile
    -   Support for more complex `edit_rule` requests
    -   Option to run calls in the background with `background = TRUE`
-   Added new functions:
    -   `cancelJob()`: cancels a previously submitted job
    -   `checkStatus()`: checks the status of a background job manually and download files if complete
    -   `healthCheck()`: checks the current status of the LFPS API
    -   `landfireVSI()`: reads LANDFIRE GeoTIFFs as a Spatraster using GDAL's Virtual File System (VSI)
-   Improved error handling and reporting and minor bug fixes
-   Updated documentation, vignette, and examples to reflect the new API
-   License for v2.0.0 is now GPL-3.0 due to dependencies on `terra`

# rlandfire 1.0.1
-   Created startup message with warning about potential/upcoming product name changes
-   Updated links to LFPS products table in documentation and `viewProducts()`
-   Updated documentation to reflect product name changes
-   Added documentation to `README.md` and the vignette on working with categorical data
-   Added `foreign` to suggests

# rlandfire 1.0.0
-   CRAN submission
-   Modified `CITATION` and `DESCRIPTION` files
-   Minor edits to vignette and documentation
-   Improved error reporting in `landfireAPI()` if multiple map zones are supplied

# rlandfire 0.4.1
-   Fixed issue with license badge in README

# rlandfire 0.4.0
  - Added new function (`getZone()`) which returns the LANDFIRE map zone based on a spatial object or zone name 
  - Removed dependencies on `terra` and `stringr`
  - Updated to MIT + file LICENSE
  - Added argument to specify order of values in the `extend` argument of `getAOI()`
  - Added additional tests for `getAOI()`, `getZone()`, and `landfireAPI()`
  - Improved portability of examples

# rlandfire 0.3.0

-   Added new function (`viewProducts()`) to open the LFPS products table in your web browser
-   Fixed bug causing early timeout well before `max_time` was reached

# rlandfire 0.2.2

-   Added a `NEWS.md` file to track changes to the package.
-   Improved getAOI() example
-   Added additional checks to landfireAPI
-   Created a vignette 

# rlandfire 0.2.1

-   Corrected issue with `getAOI()` returning values in wrong order
-   Added checks to prevent API error when resolution is set to 30 by user
-   Fixed documentation formatting
-   Updated package tests

# rlandfire 0.2.0

-   Added edit rule functionality in `landfireAPI()`
-   Improved error/warning reporting
-   Return useful object w/ class `landfire_api`
-   Added tests for `landfireAPI()` and `.fmt_editrules` (internal)
-   Improved and updated documentation
-   Fixed minor errors and bugs
