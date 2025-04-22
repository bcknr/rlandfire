# Tests for landfireAPIv2.R

test_that("`landfireAPIv2()` recognizes argument errors", {
  products <-  c("ASP2020", "ELEV2020", "230CC")
  aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
  email <- "rlandfire@markabuckner.com"
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
               "A valid `email` address is required.*")
  
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
               "argument `aoi` must be between 1 and 79.*")

  expect_error(landfireAPIv2(products, aoi = c(-200, 43, -179, 44),
                           email = email, path = path),
               "argument `aoi` must be latitude and longitude.*")
  
  expect_error(landfireAPIv2(products, aoi = c(-123, 43, -124, 44),
                           email = email, path = path),
               "argument `aoi` must be ordered `xmin`, `ymin`, `xmax`, `ymax`")
  
  expect_error(landfireAPIv2(products, aoi = c(65,66),
                           email = email, path = path),
               "argument `aoi` must be vector of coordinates.*")

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
               "`edit_rule` operator classes must only be .*")
  expect_error(landfireAPIv2(products, aoi, email = email,
                           edit_rule = list(c("condition","ELEV2020","xx",500),
                                            c("change", "230CC", "st", 181))),
               "`edit_rule` conditional operators must be one of .*")
  
  # Returns error if `edit_mask` but no edit_rule
  expect_error(landfireAPIv2(products, aoi, email = email,
                           edit_mask = testthat::test_path("testdata", "wildfire.zip"),
                           path = path),
               "`edit_mask` requires `edit_rule` to be specified.")
})

test_that("`landfireAPIv2()` returns errors with email/priority_code", {

  products <-  c("ASP2020")
  aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
  projection <- 6414
  path <- tempfile(fileext = ".zip")

  # ID errors with LFPSv1 requests with positional arguments
  expect_error(landfireAPIv2(products, aoi, projection, path = path),
               "A valid `email` address is required.*")

  # Check class
  expect_error(landfireAPIv2(products, aoi, email = "test@email.com",
                             priority_code = 1, path = path),
               "argument `priority_code` must be a character string")

})


httptest2::with_mock_dir("_mock/landfireAPI-priority", {

  products <-  c("ELEV2020", "SLPD2020", "ASP2020", "230FBFM40",
                   "230CC", "230CH", "230CBH", "230CBD")
  aoi <- c("-113.79", "42.148", "-113.56", "42.29")
  email <- "example@domain.com"
  priority_code <- "K3LS9F"
  path <- tempfile(fileext = ".zip")

  test_that("`landfireAPIv2()` formats priority requests correctly", {
    output <- landfireAPIv2(products, aoi, email,
                            priority_code = priority_code,
                            path = path)
    expect_identical(output$request$url,"https://lfps.usgs.gov/api/job/submit?Email=example%40domain.com&Layer_List=ELEV2020%3BSLPD2020%3BASP2020%3B230FBFM40%3B230CC%3B230CH%3B230CBH%3B230CBD&Area_of_Interest=-113.79%2042.148%20-113.56%2042.29&Priority_Code=K3LS9F")
  })

  test_that("`landfireAPIv2()` returns expected messages", {
    expect_message(landfireAPIv2(products, aoi, email,
                                 priority_code = priority_code),
                     "`path` is missing. Files will be saved in temporary directory: .*")

    # Return correct message with background jobs
    expect_message(landfireAPIv2(products, aoi, email,
                                 priority_code = priority_code,
                                 background = TRUE, path = path),
                   "Job submitted in background.*")
    expect_message(landfireAPIv2(products, aoi, email,
                                 priority_code = priority_code,
                                 max_time = 0, path = path),
                   "Job submitted in background.*")

  })
})


httptest2::with_mock_dir("_mock/landfireAPI-failed", {
  test_that("`landfireAPIv2()` recognizes failed call", {

    products <-  "NotAProduct"
    aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
    email <- "rlandfire@markabuckner.com"
    projection <- 123456
    path <- tempfile(fileext = ".zip")

    expect_warning(landfireAPIv2(products, aoi, email, projection, path = path),
                   paste("Job .* has failed with:\n\t.*\nPlease check the LFPS",
                   "API documentation for more information."))
  })
})

httptest2::with_mock_dir("_mock/landfireAPI-edge", {
  test_that("`landfireAPIv2()` edge cases", {

    products <-  c("ASP2020")
    aoi <- c("-123.65", "41.75", "-123.63", "41.83")
    email <- "rlandfire@markabuckner.com"
    path <- tempfile(fileext = ".zip")

    # Resets resolution to NULL when user sets resolution = 30
    result  <- landfireAPIv2(products, aoi, email,
                            resolution = 30, path = path)
    expect_null(result$request$query$resolution)
  })
})

httptest2::with_mock_dir("_mock/landfireAPI-aoi", {
  test_that("`landfireAPIv2()` works with `getAOI()`", {

    products <-  c("ASP2020")
    email <- "rlandfire@markabuckner.com"
    path <- tempfile(fileext = ".zip")

    r <- terra::rast(nrows = 50, ncols = 50,
                    xmin = -2261174.94, xmax = -2247816.36,
                    ymin = 2412704.65, ymax = 2421673.98,
                    crs = terra::crs("epsg:5070"),
                    vals = rnorm(2500))

    aoi <- round(getAOI(r),3)

    aoi_result <- landfireAPIv2(products = products, aoi = aoi,
                                email = email, path = path)


    expect_identical(aoi_result$request$query$Area_of_Interest,
                     "-123.803 41.723 -123.616 41.834")
  })
})

httptest2::with_mock_dir("_mock/landfireAPI-zone", {
  test_that("`landfireAPIv2()` works with `getZone()`", {

    products <-  c("ASP2020")
    email <- "rlandfire@markabuckner.com"
    resolution <- 90
    path <- tempfile(fileext = ".zip")

    zone <- getZone("Northern California Coastal Range")

    zone_result <- landfireAPIv2(products = products, aoi = zone,
                                 email = email, resolution = resolution,
                                 path = path)
    expect_identical(zone_result$request$query$Area_of_Interest,
                     "3")
  })
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
  expect_null(.post_editmask(NULL)$item_id)
  expect_null(.post_editmask(NULL)$item_name)
})

# Tests for .post_editmask (internal)
test_that("`.post_editmask` returns expected response", {

  skip_on_cran()

  shapefile <- testthat::test_path("testdata", "wildfire.zip")
  result <- .post_editmask(shapefile)

  expect_match(result$item_id, "[{\"itemID\":\".*\"}]")
  expect_match(result$item_name, "wildfire.shp$")

})

# Tests for .fmt_editrules (internal)
test_that("`.fmt_editrules` correctly formats requests",{

  # One condition, one change
  single_rule <- list(c("condition", "ELEV2020", "lt", 500),
                      c("change", "230CC", "st", 181))
  expect_identical(.fmt_editrules(single_rule),
                   "{\"edit\":[{\"condition\":[{\"product\":\"ELEV2020\",\"operator\":\"lt\",\"value\":500}],\"change\":[{\"product\":\"230CC\",\"operator\":\"st\",\"value\":181}]}]}")

  # Multiple conditions
  multi_rule <- list(c("condition", "ELEV2020", "lt", 500),
                     c("change", "230CC", "st", 181),
                     c("condition", "ELEV2020", "ge", 600),
                     c("change", "230CC", "db", 20),
                     c("condition", "ELEV2020", "eq", 550),
                     c("change", "230CC", "st", 0))
  expect_identical(.fmt_editrules(multi_rule),
                   "{\"edit\":[{\"condition\":[{\"product\":\"ELEV2020\",\"operator\":\"lt\",\"value\":500}],\"change\":[{\"product\":\"230CC\",\"operator\":\"st\",\"value\":181}],\"condition\":[{\"product\":\"ELEV2020\",\"operator\":\"ge\",\"value\":600}],\"change\":[{\"product\":\"230CC\",\"operator\":\"db\",\"value\":20}],\"condition\":[{\"product\":\"ELEV2020\",\"operator\":\"eq\",\"value\":550}],\"change\":[{\"product\":\"230CC\",\"operator\":\"st\",\"value\":0}]}]}")

  
  # Single condition with multiple changes (Pulled from documentation)
  multi_change  <- list(c("condition", "ELEV2020", "lt", 500),
                        c("change", "140FBFM", "st", 181),
                        c("change", "140CBH", "ib", 5))
    expect_identical(.fmt_editrules(multi_change),
                   "{\"edit\":[{\"condition\":[{\"product\":\"ELEV2020\",\"operator\":\"lt\",\"value\":500}],\"change\":[{\"product\":\"140FBFM\",\"operator\":\"st\",\"value\":181},{\"product\":\"140CBH\",\"operator\":\"ib\",\"value\":5}]}]}")
  
  # Multiple conditions with OR (Pulled from documentation)
  or_rule  <- list(c("condition", "ELEV2020", "", 0),
                  c("change", "140FBFM", "st", 181),
                  c("change", "140CBH", "ib", 5),
                  c("ORcondition", "ELEV2020", "gt", 500),
                  c("condition", "ELEV2020", "lt", 600),
                  c("change", "140FBFM", "st", 181),
                  c("change", "140CBH", "ib", 5))
  expect_identical(.fmt_editrules(or_rule),
                   "{\"edit\":[{\"condition\":[{\"product\":\"ELEV2020\",\"operator\":\"\",\"value\":0}],\"change\":[{\"product\":\"140FBFM\",\"operator\":\"st\",\"value\":181},{\"product\":\"140CBH\",\"operator\":\"ib\",\"value\":5}]}],\"edit\":[{\"condition\":[{\"product\":\"ELEV2020\",\"operator\":\"gt\",\"value\":500},{\"product\":\"ELEV2020\",\"operator\":\"lt\",\"value\":600}],\"change\":[{\"product\":\"140FBFM\",\"operator\":\"st\",\"value\":181},{\"product\":\"140CBH\",\"operator\":\"ib\",\"value\":5}]}]}")

  # Single `edit_mask` with simple edit rules
  single_mask  <- list(c("condition", "ELEV2020", "eq", 593),
                      c("change", "140CC", "st", 500),
                      c("change", "140CH", "ib", 50))
  mask  <- list(item_id = "{\"itemID\":\"i5ce09134-4e57-41fe-bcaa-0c38879bc3fc\"}]",
                item_name = "wildfire.shp")
  expect_identical(.fmt_editrules(single_mask, mask),
                   "{\"edit\":[{\"mask\":\"wildfire.shp\",\"condition\":[{\"product\":\"ELEV2020\",\"operator\":\"eq\",\"value\":593}],\"change\":[{\"product\":\"140CC\",\"operator\":\"st\",\"value\":500},{\"product\":\"140CH\",\"operator\":\"ib\",\"value\":50}]}]}")
  
  # Returns NULL if no file is provided
  expect_null(.fmt_editrules(NULL))
})
