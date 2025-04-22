# Test for utils.R

# `landfireVSI()`

test_that("landfireVSI() works as expected", {
    expect_error(
        landfireVSI("not a landfire_api object"),
        "argument `landfire_api` must be a landfire_api object"
    )
    expect_error(
        landfireVSI(.build_landfire_api(),
        "The provided `landfire_api` object does not contain a valid `path` or `dwl_url`")
    )

    expect_error(
        landfireVSI(.build_landfire_api(path = test_path("testdata", "vsiTestdoesntexist.zip"))),
        "No file associated with the provide `landfire_api` object was found"
    )

    expect_s4_class(
        landfireVSI(.build_landfire_api(path = test_path("testdata", "vsiTest.zip"))),
        "SpatRaster"
    )
})

test_that("landfireVSI() works with a valid url", {
    skip_on_cran()

    aoi  <- c("-113.79", "42.148", "-113.56", "42.29")
    email <- "rlandfire@markabuckner.com"
    resp <- landfireAPIv2(products = "240EVC",
                          aoi = aoi, email = email,
                          method = "none")
    
    expect_s4_class(landfireVSI(resp), "SpatRaster")

})