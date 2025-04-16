# Tests for landfireAPIv2.R

test_that("`landfireAPIv2()` recognizes argument errors", {
  products <-  c("ASP2020", "ELEV2020", "230CC")
  aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
  email <- "mab677@cornell.edu"
  projection <- 6414
  resolution <- 90
  edit_rule <- list(c("condition", "ELEV2020", "lt", 500),
                    c("change", "230CC", "st", 181))
  path <- tempfile(fileext = ".zip")

  # Check for required arguments
  expect_error(landfireAPIv2(aoi = aoi, email = email),
               "argument `products` is missing with no default")

  expect_error(landfireAPIv2(products, email = email),
               "argument `aoi` is missing with no default")

  expect_error(landfireAPIv2(products, aoi),
               'argument "email" is missing, with no default')

  # Check class
  expect_error(landfireAPIv2(products = c(1,2,3), aoi,
                           email = email, path = path),
               "argument `products` must be a character vector")
  
  expect_error(landfireAPIv2(products, aoi = list(1,2,3),
                           email = email, path = path),
               "argument `aoi` must be a character or numeric vector")
  
  expect_error(landfireAPIv2(products, aoi, email = "notanemail"),
               "A valid `email` address is required. (See `?rlandfire::landfireAPIv2` for more information)",
               fixed = TRUE)
  
  expect_error(landfireAPIv2(products, aoi, email = email,
                           max_time = TRUE, path = path),
               "argument `max_time` must be numeric")
  
  expect_error(landfireAPIv2(products, aoi, email = email,
                           verbose = "yes", path = path),
               "argument `verbose` must be logical")
  
  expect_error(landfireAPIv2(products, aoi, email = email,
                           edit_rule = "edit_rule"),
               "argument `edit_rule` must be a list")

  # Check `aoi` errors
  expect_error(landfireAPIv2(products, aoi = 100, email = email, path = path),
               "argument `aoi` must be between 1 and 79 if using LANDFIRE map zones")

  expect_error(landfireAPIv2(products, aoi = c(-200, 43, -179, 44),
                           email = email, path = path),
               "argument `aoi` must be latitude and longitude in decimal degrees (WGS84) or a LANDFIRE map zone",
               fixed = TRUE)
  
  expect_error(landfireAPIv2(products, aoi = c(-123, 43, -124, 44),
                           email = email, path = path),
               "argument `aoi` must be ordered `xmin`, `ymin`, `xmax`, `ymax`")
  
  expect_error(landfireAPIv2(products, aoi = c(65,66),
                           email = email, path = path),
               "argument `aoi` must be vector of coordinates with length == 4 or a single map zone")

  # Check `resolution`
  expect_error(landfireAPIv2(products, aoi, email = email,
                           resolution = 20, path = path),
               "argument `resolution` must be between 30 and 9999 or `NULL`")
  expect_error(landfireAPIv2(products, aoi, email = email,
                           resolution = 10000, path = path),
               "argument `resolution` must be between 30 and 9999 or `NULL`")

  # Check edit_rule arguments
  expect_error(landfireAPIv2(products, aoi, email = email,
                           edit_rule = list(c("wrong","ELEV2020","lt",500),
                                            c("change", "230CC", "st", 181))),
               '`edit_rule` operator classes must only be "condition" or "change"')
  expect_error(landfireAPIv2(products, aoi, email = email,
                           edit_rule = list(c("condition","ELEV2020","xx",500),
                                            c("change", "230CC", "st", 181))),
               '`edit_rule` conditional operators must be one of "eq","ge","gt","le","lt","ne"')
})

test_that("`landfireAPIv2()` returns helpful errors with email/priority_code", {

  products <-  c("ASP2020")
  aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
  projection <- 6414
  path <- tempfile(fileext = ".zip")

  # ID errors with LFPSv1 requests with positional arguments
  expect_error(landfireAPIv2(products, aoi, projection, path = path),
               "A valid `email` address is required. (See `?rlandfire::landfireAPIv2` for more information)",
               fixed = TRUE)

  # Check class
  expect_error(landfireAPIv2(products, aoi, email = "test@email.com",
                             priority_code = 1, path = path),
               "argument `priority_code` must be a character string")

})

httptest2::with_mock_dir("_mock/landfireAPI-priority", {
  test_that("`landfireAPIv2()` formats priority requests correctly", {
    products <-  c("ELEV2020", "SLPD2020", "ASP2020", "230FBFM40",
                   "230CC", "230CH", "230CBH", "230CBD")
    aoi <- c("-113.79", "42.148", "-113.56", "42.29")
    email <- "example@domain.com"
    priority_code <- "K3LS9F"
    path <- tempfile(fileext = ".zip")

    output <- landfireAPIv2(products, aoi, email,
                            priority_code = priority_code,
                            path = path)
    expect_identical(output$request$url,"https://lfps.usgs.gov/api/job/submit?Email=example%40domain.com&Layer_List=ELEV2020%3BSLPD2020%3BASP2020%3B230FBFM40%3B230CC%3B230CH%3B230CBH%3B230CBD&Area_of_Interest=-113.79%2042.148%20-113.56%2042.29&Priority_Code=K3LS9F")
  })
})

# httptest2::with_mock_dir("_mock/landfireAPI-messages", {
#   test_that("`landfireAPIv2()` formats priority requests correctly", {
#     products <-  c("ELEV2020", "SLPD2020", "ASP2020", "230FBFM40",
#                    "230CC", "230CH", "230CBH", "230CBD")
#     aoi <- c("-113.79", "42.148", "-113.56", "42.29")
#     email <- "example@domain.com"
#     path <- tempfile(fileext = ".zip")

#     expect_warning(landfireAPIv2(products, aoi, email),
#     "`path` is missing. Files will be saved in temporary directory: .*.zip")

#     expect_warning(landfireAPIv2(products, aoi, email, path = path,
#                    background = TRUE),
#     paste("Job submitted in background.\n",
#               "Call `checkStatus()` to check the current status and download",
#               " if completed.\n",
#               "Or visit URL to check status and download manually:\n   .*"))

#     expect_warning(landfireAPIv2(products, aoi, email, path = path,
#                    max_time = 5),
#     paste("Job status: Incomplete and `max_time` reached\n",
#               "Call: `checkStatus()` to check the current status\n",
#               "      `cancelJob()` to cancel.\n",
#               "Or visit URL to check status and download manually:\n   .*"))

#   })
# })

httptest2::with_mock_dir("_mock/landfireAPI-failed", {
  test_that("`landfireAPIv2()` recognizes failed call", {

    products <-  "NotAProduct"
    aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
    email <- "mab677@cornell.edu"
    projection <- 123456
    path <- tempfile(fileext = ".zip")

    expect_warning(landfireAPIv2(products, aoi, email, projection, path = path),
                   paste("Job .* has failed with:\n\t.*\nPlease check the LFPS",
                   "API documentation for more information."))
  })
})

test_that("`landfireAPIv2()` edge cases", {

  skip_on_cran()

  products <-  c("ASP2020")
  aoi <- c("-123.65", "41.75", "-123.63", "41.83")
  email <- "mab677@cornell.edu"
  resolution <- 90
  path <- tempfile(fileext = ".zip")

  # Functions when resolution = 30
  expect_no_error(landfireAPIv2(products, aoi, email,
                              resolution = 30, path = path))
})


test_that("`landfireAPIv2()` works with `getAOI()` and `getZone()`", {

  skip_on_cran()

  products <-  c("ASP2020")
  email <- "mab677@cornell.edu"
  resolution <- 90
  path <- tempfile(fileext = ".zip")

  r <- terra::rast(nrows = 50, ncols = 50,
                   xmin = -2261174.94, xmax = -2247816.36,
                   ymin = 2412704.65, ymax = 2421673.98,
                   crs = terra::crs("epsg:5070"),
                   vals = rnorm(2500))

  aoi <- getAOI(r)
  zone <- getZone("Northern California Coastal Range")

  expect_no_error(landfireAPIv2(products = products, aoi = aoi, email = email,
                              resolution = resolution, path = path))
  expect_no_error(landfireAPIv2(products = products, aoi = zone, email = email,
                              resolution = resolution, path = path))
})

# Tests for .post_request (internal)
test_that("`.post_request` catches file issues", {
  expect_error(.post_editmask("notafile.zip"),
               "`edit_mask` file not found")

  # Check for file extension
  tmp_file <- tempfile(fileext = ".txt")
  writeBin(raw(32 * 32), tmp_file)
  expect_error(.post_editmask(tmp_file),
               "`edit_mask` file must be a zipped shapefile (.zip)",
               fixed = TRUE)

  # Check for file size
  writeBin(raw(1024 * 1024 + 1), tmp_file)
  expect_error(.post_editmask(tmp_file),
               "`edit_mask` file exceeds maximum allowable size (1MB)",
               fixed = TRUE)
  unlink(tmp_file)

  # Check for shapefile
  expect_error(.post_editmask(testthat::test_path("testdata", "editmask_noshp.zip")),
               "`edit_mask` file does not contain a shapefile")
  
  # Returns NULL if no file is provided
  expect_null(.post_editmask(NULL))
})


test_that("`.post_editmask` returns expected response", {

  skip_on_cran()

  shapefile <- testthat::test_path("testdata", "Wildfire_History.zip")
  result <- .post_editmask(shapefile)

  expect_match(result, '\"itemName\":\"Wildfire_History.zip\"')
  expect_match(result, '\"description\":null')
  expect_match(result, '\"committed\":true')
  expect_match(result, '\"success\":true')

})



# Tests for .fmt_editrules (internal)
test_that("`.fmt_editrules` correctly formats requests",{
  single_rule <- list(c("condition", "ELEV2020", "lt", 500),
                      c("change", "230CC", "st", 181))
  multi_rule <- list(c("condition", "ELEV2020", "lt", 500),
                     c("change", "230CC", "st", 181),
                     c("condition", "ELEV2020", "ge", 600),
                     c("change", "230CC", "db", 20),
                     c("condition", "ELEV2020", "eq", 550),
                     c("change", "230CC", "st", 0))

  expect_identical(.fmt_editrules(single_rule),
                   "{\"edit\":[{\"condition\":[{\"product\":\"ELEV2020\",\"operator\":\"lt\",\"value\":500}],\"change\":[{\"product\":\"230CC\",\"operator\":\"st\",\"value\":181}]}]}")
  expect_identical(.fmt_editrules(multi_rule),
                   "{\"edit\":[{\"condition\":[{\"product\":\"ELEV2020\",\"operator\":\"lt\",\"value\":500}],\"change\":[{\"product\":\"230CC\",\"operator\":\"st\",\"value\":181}],\"condition\":[{\"product\":\"ELEV2020\",\"operator\":\"ge\",\"value\":600}],\"change\":[{\"product\":\"230CC\",\"operator\":\"db\",\"value\":20}],\"condition\":[{\"product\":\"ELEV2020\",\"operator\":\"eq\",\"value\":550}],\"change\":[{\"product\":\"230CC\",\"operator\":\"st\",\"value\":0}]}]}")

  # Returns NULL if no file is provided
  expect_null(.fmt_editrules(NULL))
})
