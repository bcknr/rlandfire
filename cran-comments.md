## Update (v2.0.1)

* This is a patch to the existing `rlandfire` package. To address issues resulting in failed checks on CRAN.

* Changes include:

    - All tests that require API access are mocked when possible or conditionally skipped with `skip_on_cran()`.
    - Tests expected to fail prior to making an API call are now wrapped in `without_internet()` to prevent unexpected calls.
    - The old `landfireAPI` function has been fully deprecated. 

## R CMD check results

0 errors | 0 warnings | 0 notes

* Possibly mis-spelled words in DESCRIPTION:
    LANDFIRE (3:21)
    geospatial (12:44)
    
  Both of the flagged words are spelled correctly.

* Tests requiring API access are conditionally skipped or mocked.