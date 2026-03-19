# Tests for landfireAPIv2.R

test_that("`landfireAPIv2()` recognizes argument errors", {
  products <- c("LF2016_CC", "LF2016_CBH")
  aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
  email <- "rlandfire@markabuckner.com"
  projection <- 6414
  resolution <- 90
  edit_rule <- list(
    c("condition", "LF2016_CC", "lt", 50),
    c("change", "LF2016_CBH", "st", 0)
  )
  path <- tempfile(fileext = ".zip")

  # Check for required arguments
  expect_error(
    landfireAPIv2(aoi = aoi, email = email, execute = FALSE),
    "argument `products` is missing with no default"
  )

  expect_error(
    landfireAPIv2(products, email = email, execute = FALSE),
    "argument `aoi` is missing with no default"
  )

  expect_error(
    landfireAPIv2(products, aoi, execute = FALSE),
    'argument "email" is missing, with no default'
  )

  # Check class
  expect_error(
    landfireAPIv2(
      products = c(1, 2, 3), aoi,
      email = email, path = path,
      execute = FALSE
    ),
    "argument `products` must be a character vector"
  )

  expect_error(
    landfireAPIv2(products,
      aoi = list(1, 2, 3),
      email = email, path = path, execute = FALSE
    ),
    "argument `aoi` must be a character or numeric vector"
  )

  expect_error(
    landfireAPIv2(products, aoi, email = "notanemail", execute = FALSE),
    "A valid `email` address is required.*"
  )

  expect_error(
    landfireAPIv2(products, aoi,
      email = email,
      max_time = TRUE, path = path, execute = FALSE
    ),
    "argument `max_time` must be numeric"
  )

  expect_error(
    landfireAPIv2(products, aoi,
      email = email,
      verbose = "yes", path = path, execute = FALSE
    ),
    "argument `verbose` must be logical"
  )

  expect_error(
    landfireAPIv2(products, aoi,
      email = email,
      edit_rule = "edit_rule", execute = FALSE
    ),
    "argument `edit_rule` must be a list"
  )

  # Check `aoi` errors
  expect_error(
    landfireAPIv2(products,
      aoi = 100, email = email,
      path = path, execute = FALSE
    ),
    "argument `aoi` must be between 1 and 79.*"
  )

  expect_error(
    landfireAPIv2(products,
      aoi = c(-200, 43, -179, 44),
      email = email, path = path, execute = FALSE
    ),
    "argument `aoi` must be latitude and longitude.*"
  )

  expect_error(
    landfireAPIv2(products,
      aoi = c(-123, 43, -124, 44),
      email = email, path = path, execute = FALSE
    ),
    "argument `aoi` must be ordered `xmin`, `ymin`, `xmax`, `ymax`"
  )

  expect_error(
    landfireAPIv2(products,
      aoi = c(65, 66),
      email = email, path = path, execute = FALSE
    ),
    "argument `aoi` must be vector of coordinates.*"
  )

  # Check `resolution`
  expect_error(
    landfireAPIv2(products, aoi,
      email = email,
      resolution = 20, path = path, execute = FALSE
    ),
    "argument `resolution` must be between 30 and 9999 or `NULL`"
  )
  expect_error(
    landfireAPIv2(products, aoi,
      email = email,
      resolution = 10000, path = path, execute = FALSE
    ),
    "argument `resolution` must be between 30 and 9999 or `NULL`"
  )

  # Check edit_rule arguments
  expect_error(
    landfireAPIv2(products, aoi,
      email = email,
      edit_rule = list(
        c("wrong", "LF2016_CC", "lt", 50),
        c("change", "LF2016_CBH", "st", 0)
      ), execute = FALSE
    ),
    "`edit_rule` operator classes must only be .*"
  )
  expect_error(
    landfireAPIv2(products, aoi,
      email = email,
      edit_rule = list(
        c("condition", "LF2016_CC", "xx", 50),
        c("change", "LF2016_CBH", "st", 0)
      ), execute = FALSE
    ),
    "`edit_rule` conditional operators must be one of .*"
  )

  # Returns error if `edit_mask` but no edit_rule
  expect_error(
    landfireAPIv2(products, aoi,
      email = email,
      edit_mask = testthat::test_path("testdata", "wildfire.zip"),
      path = path, execute = FALSE
    ),
    "`edit_mask` requires `edit_rule` to be specified."
  )
})

test_that("`landfireAPIv2()` returns errors with email/priority_code", {
  products <- c("LF2016_CC")
  aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
  projection <- 6414
  path <- tempfile(fileext = ".zip")

  # ID errors with LFPSv1 requests with positional arguments
  expect_error(
    landfireAPIv2(products, aoi, projection,
      path = path,
      execute = FALSE
    ),
    "A valid `email` address is required.*"
  )

  # Check class
  expect_error(
    landfireAPIv2(products, aoi,
      email = "test@email.com",
      priority_code = 1, path = path, execute = FALSE
    ),
    "argument `priority_code` must be a character string"
  )
})



test_that("`landfireAPIv2()` formats priority requests correctly", {
  products <- c(
    "LF2016_CC", "LF2020_SLPD", "LF2020_ASP", "LF2022_FBFM40",
    "LF2022_CC", "LF2022_CH", "LF2022_CBH", "LF2022_CBD"
  )
  aoi <- c("-113.79", "42.148", "-113.56", "42.29")
  email <- "example@domain.com"
  priority_code <- "K3LS9F"
  path <- tempfile(fileext = ".zip")

  output <- landfireAPIv2(products, aoi, email,
    priority_code = priority_code,
    path = path, method = "none",
    execute = FALSE
  )
  expect_identical(output$request$url, "https://lfps.usgs.gov/api/job/submit?Email=example%40domain.com&Layer_List=LF2016_CC%3BLF2020_SLPD%3BLF2020_ASP%3BLF2022_FBFM40%3BLF2022_CC%3BLF2022_CH%3BLF2022_CBH%3BLF2022_CBD&Area_of_Interest=-113.79%2042.148%20-113.56%2042.29&Priority_Code=K3LS9F")
})


test_that("`landfireAPIv2()` returns expected messages", {
  products <- c("LF2016_CC")
  aoi <- c("-113.79", "42.148", "-113.56", "42.29")
  email <- "rlandfire@markabuckner.com"
  path <- "path.zip"
  
  expect_message(
    landfireAPIv2(products, aoi, email, background = TRUE, execute = FALSE),
    "`path` is missing. Files will be saved in temporary directory: .*"
  )
})


test_that("`landfireAPIv2()` returns expected background messages", {
  skip_on_cran()

  products <- c("LF2016_CC")
  aoi <- c("-113.79", "42.148", "-113.56", "42.29")
  email <- "rlandfire@markabuckner.com"
  path <- "path.zip"
  # Return correct message with background jobs
  expect_message(
    output <- landfireAPIv2(products, aoi, email,
      background = TRUE, path = path,
      method = "none", verbose = FALSE,
    ),
    "Job submitted in background.*"
  )

  expect_message(
    cancelJob(output$request$job_id),
    "Job .* has been canceled"
  )


  expect_message(
    output <- landfireAPIv2(products, aoi, email,
      max_time = 0, path = path,
      method = "none", verbose = FALSE
    ),
    "Job submitted in background.*"
  )

  cancelJob(output$request$job_id)
})



test_that("`landfireAPIv2()` recognizes failed call", {
  skip_on_cran()
  products <- "NotAProduct"
  aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
  email <- "rlandfire@markabuckner.com"
  projection <- 123456
  path <- tempfile(fileext = ".zip")

  expect_warning(
    landfireAPIv2(products, aoi, email, projection, path = path),
    paste(
      "Job .* has failed with:\n\t.*\nPlease check the LFPS",
      "API documentation for more information."
    )
  )
})




test_that("`landfireAPIv2()` edge cases", {
  products <- c("LF2016_CC")
  aoi <- c("-123.65", "41.75", "-123.63", "41.83")
  email <- "rlandfire@markabuckner.com"
  path <- tempfile(fileext = ".zip")

  # Resets resolution to NULL when user sets resolution = 30
  result <- landfireAPIv2(products, aoi, email,
    resolution = 30, path = path,
    method = "none", execute = FALSE
  )
  expect_null(result$request$query$resolution)
})



test_that("`landfireAPIv2()` works with `getAOI()`", {
  products <- c("LF2016_CC")
  email <- "rlandfire@markabuckner.com"
  path <- tempfile(fileext = ".zip")

  r <- terra::rast(
    nrows = 50, ncols = 50,
    xmin = -2261174.94, xmax = -2247816.36,
    ymin = 2412704.65, ymax = 2421673.98,
    crs = terra::crs("epsg:5070"),
    vals = rnorm(2500)
  )

  aoi <- round(getAOI(r), 3)

  aoi_result <- landfireAPIv2(
    products = products, aoi = aoi,
    email = email, path = path,
    method = "none", execute = FALSE
  )


  expect_identical(
    aoi_result$request$query$Area_of_Interest,
    "-123.803 41.723 -123.616 41.834"
  )
})



test_that("`landfireAPIv2()` works with `getZone()`", {
  products <- c("LF2020_ASP")
  email <- "rlandfire@markabuckner.com"
  resolution <- 90
  path <- tempfile(fileext = ".zip")

  zone <- getZone("Northern California Coastal Range")

  zone_result <- landfireAPIv2(
    products = products, aoi = zone,
    email = email, resolution = resolution,
    path = path, method = "none", execute = FALSE
  )
  expect_identical(
    zone_result$request$query$Area_of_Interest,
    "3"
  )
})


# Tests for .post_request (internal)
test_that("`.post_request` catches file issues", {
  httptest2::without_internet({
    expect_error(
      .post_editmask("notafile.zip"),
      "`edit_mask` file not found"
    )

    # Check for file extension
    tmp_file <- tempfile(fileext = ".txt")
    writeBin(raw(32 * 32), tmp_file)
    expect_error(.post_editmask(tmp_file),
      "`edit_mask` file must be a zipped shapefile (.zip)",
      fixed = TRUE
    )

    # Check for file size
    writeBin(raw(1024 * 1024 + 1), tmp_file)
    expect_error(.post_editmask(tmp_file),
      "`edit_mask` file exceeds maximum allowable size (1MB)",
      fixed = TRUE
    )
    unlink(tmp_file)

    # Check for shapefile
    expect_error(
      .post_editmask(testthat::test_path("testdata", "editmask_noshp.zip")),
      "`edit_mask` file does not contain a shapefile"
    )

    # Returns NULL if no file is provided
    expect_null(.post_editmask(NULL)$item_id)
    expect_null(.post_editmask(NULL)$item_name)
  })
})

# Tests for .post_editmask (internal)
test_that("`.post_editmask` returns expected response", {
  skip_on_cran()

  shapefile <- testthat::test_path("testdata", "wildfire.zip")
  result <- rlandfire:::.post_editmask(shapefile)

  expect_match(result$item_id, "[{\"itemID\":\".*\"}]")
  expect_match(result$item_name, "wildfire.shp$")
})


# Tests for .fmt_editrules (internal)
# These are nonsense but they meet the goal
test_that("`.fmt_editrules` correctly formats requests", {

  products <- c("LF2016_CC", "LF2016_CBH")
  aoi <- c("-123.65", "41.75", "-123.63", "41.83")
  email <- "rlandfire@markabuckner.com"
  
  # One condition, one change
  single_rule <- list(
    c("condition", "LF2016_CC", "lt", 50),
    c("change", "LF2016_CBH", "st", 0)
  )
  expect_identical(
    .fmt_editrules(single_rule),
    "{\"edit\":[{\"condition\":[{\"product\":\"LF2016_CC\",\"operator\":\"lt\",\"value\":50}],\"change\":[{\"product\":\"LF2016_CBH\",\"operator\":\"st\",\"value\":0}]}]}"
  )

  # Multiple conditions
  multi_rule <- list(
    c("condition", "LF2016_CC", "lt", 15),
    c("change", "LF2016_CBH", "st", 0),
    c("condition", "LF2016_CC", "ge", 50),
    c("change", "LF2016_CBH", "db", 2),
    c("condition", "LF2016_CC", "eq", 100),
    c("change", "LF2016_CBH", "st", 1000)
  )
  expect_identical(
    .fmt_editrules(multi_rule),
    "{\"edit\":[{\"condition\":[{\"product\":\"LF2016_CC\",\"operator\":\"lt\",\"value\":15}],\"change\":[{\"product\":\"LF2016_CBH\",\"operator\":\"st\",\"value\":0}],\"condition\":[{\"product\":\"LF2016_CC\",\"operator\":\"ge\",\"value\":50}],\"change\":[{\"product\":\"LF2016_CBH\",\"operator\":\"db\",\"value\":2}],\"condition\":[{\"product\":\"LF2016_CC\",\"operator\":\"eq\",\"value\":100}],\"change\":[{\"product\":\"LF2016_CBH\",\"operator\":\"st\",\"value\":1000}]}]}"
  )


  # Single condition with multiple changes
  products <- c("LF2016_CC", "LF2016_FBFM40", "LF2016_CBH")

  multi_change <- list(
    c("condition", "LF2016_CC", "lt", 500),
    c("change", "LF2016_FBFM40", "st", 181),
    c("change", "LF2016_CBH", "ib", 5)
  )
  expect_identical(
    .fmt_editrules(multi_change),
    "{\"edit\":[{\"condition\":[{\"product\":\"LF2016_CC\",\"operator\":\"lt\",\"value\":500}],\"change\":[{\"product\":\"LF2016_FBFM40\",\"operator\":\"st\",\"value\":181},{\"product\":\"LF2016_CBH\",\"operator\":\"ib\",\"value\":5}]}]}"
  )

  # Multiple conditions with OR
  or_rule <- list(
    c("condition", "LF2016_CC", "eq", 0),
    c("change", "LF2016_FBFM40", "st", 181),
    c("change", "LF2016_CBH", "ib", 5),
    c("ORcondition", "LF2016_CC", "gt", 50),
    c("condition", "LF2016_CC", "lt", 60),
    c("change", "LF2016_FBFM40", "st", 181),
    c("change", "LF2016_CBH", "ib", 5)
  )
  expect_identical(
    .fmt_editrules(or_rule),
    "{\"edit\":[{\"condition\":[{\"product\":\"LF2016_CC\",\"operator\":\"eq\",\"value\":0}],\"change\":[{\"product\":\"LF2016_FBFM40\",\"operator\":\"st\",\"value\":181},{\"product\":\"LF2016_CBH\",\"operator\":\"ib\",\"value\":5}]}],\"edit\":[{\"condition\":[{\"product\":\"LF2016_CC\",\"operator\":\"gt\",\"value\":50},{\"product\":\"LF2016_CC\",\"operator\":\"lt\",\"value\":60}],\"change\":[{\"product\":\"LF2016_FBFM40\",\"operator\":\"st\",\"value\":181},{\"product\":\"LF2016_CBH\",\"operator\":\"ib\",\"value\":5}]}]}"
  )

  # Single `edit_mask` with simple edit rules
  products <- c("LF2016_CC", "LF2016_CH")
  single_rule <- list(
    c("condition", "LF2016_CC", "ge", 50),
    c("change", "LF2016_CC", "st", 50),
    c("change", "LF2016_CH", "ib", 5)
  )
  mask <- list(
    item_id = "{\"itemID\":\"i33f34c28-e953-4504-a979-6de40495410e\"}]",
    item_name = "wildfire.shp"
  )
  expect_identical(
    .fmt_editrules(single_rule, mask),
    "{\"edit\":[{\"mask\":\"wildfire.shp\",\"condition\":[{\"product\":\"LF2016_CC\",\"operator\":\"ge\",\"value\":50}],\"change\":[{\"product\":\"LF2016_CC\",\"operator\":\"st\",\"value\":50},{\"product\":\"LF2016_CH\",\"operator\":\"ib\",\"value\":5}]}]}"
  )

  # Correctly formats without value when "cv"
  cv_rule <- list(
    c("condition", "LF2016_CC", "le", 50),
    c("change", "LF2016_CH", "cv")
  )

  expect_identical(
    .fmt_editrules(cv_rule),
      "{\"edit\":[{\"condition\":[{\"product\":\"LF2016_CC\",\"operator\":\"le\",\"value\":50}],\"change\":[{\"product\":\"LF2016_CH\",\"operator\":\"cv\"}]}]}"
  )

  # Returns NULL if no file is provided
  expect_null(.fmt_editrules(NULL))
})
