## Update (v2.0.0)

* This is a major update to the existing `rlandfire` package. 

* Changes include:

    - Update to the new LANDFIRE Product Suite (LFPS) API
    - New functions: `cancelJob()`, `checkStatus()`, `healthCheck()`, `landfireVSI()`
    - Updated documentation to reflect/warn users of ongoing changes
    - Added documentation to `README.md` and the vignette on working with categorical data, edit mask, and vsi

## R CMD check results

0 errors | 0 warnings | 0 notes

* Possibly mis-spelled words in DESCRIPTION:
    LANDFIRE (3:21)
    geospatial (12:44)
    
  Both of the flagged words are spelled correctly.

* Some tests requiring API access are conditionally skipped.