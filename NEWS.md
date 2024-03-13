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
