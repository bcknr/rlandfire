# Test for utils.R

# `landfireVSI()`

test_that("landfireVSI() works as expected", {
    expect_error(
        landfireVSI("not a landfire_api object"),
        "argument `landfire_api` must be a landfire_api object"
    )
    expect_error(
        landfireVSI(.build_landfire_api(),
        "the provided `landfire_api` object does not contain a valid `path` or `dwl_url`")
    )

    expect_s4_class(
        landfireVSI(.build_landfire_api(path = test_path("testdata", "vsiTest.zip"))),
        "SpatRaster"
    )
})