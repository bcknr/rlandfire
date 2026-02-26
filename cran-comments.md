## Update (v2.0.2)

* This is a minor update to the existing `rlandfire` package to ensure compatability with the LFPS API's new naming convensions.

* Changes include:

    - All product names were updated in vignette, examples, and tests
    - A minor bug causing an erroneous error in `landfireAPIv2()` when `edit_rule` was provided but `edit_mask` is not.
    - Documentation was updated to better reflect the current suggested workflow for the package.

## R CMD check results

0 errors | 0 warnings | 0 notes

* Possibly mis-spelled words in DESCRIPTION:
    LANDFIRE (3:21)
    geospatial (12:44)
    
  Both of the flagged words are spelled correctly.

* Tests requiring API access are conditionally skipped or mocked.