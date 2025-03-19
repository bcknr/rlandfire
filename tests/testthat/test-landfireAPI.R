# Tests for landfireAPIv2.R

test_that("`landfireAPIv2()` recognizes arguement errors", {
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
               "argument `aoi` must be between 1 and 79 when using LANDFIRE map zones")

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

  # ID errors with LFPSv1 requests with positional arguments
  expect_error(landfireAPIv2(products, aoi, projection),
               "A valid `email` address is required. (See `?rlandfire::landfireAPIv2` for more information)",
               fixed = TRUE)

  # Check class
  expect_error(landfireAPIv2(products, aoi, email = "test@email.com",
                           priority_code = 1),
               "argument `priority_code` must be a character string")

})

# TODO: Check that URL is built correctly with email and priority_code


test_that("`landfireAPIv2()` recognizes failed call", {

  skip_on_cran()

  products <-  "NotAProduct"
  aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
  email <- "mab677@cornell.edu"
  projection <- 123456
  path <- tempfile(fileext = ".zip")

  expect_warning(landfireAPIv2(products, aoi, email, projection, path = path),
                 "Job Status:  esriJobFailed")
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

# Tests for cancelJob()
test_that("`cancelJob()` recognizes arguement errors", {
  expect_error(cancelJob(),
               "argument `job_id` is missing with no default")

  expect_error(cancelJob(job_id = "notanid"),
               "argument `job_id` must be a valid job ID")
})

# Tests for healthCheck()
# TODO: Test that healthCheck() returns a message/warning


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
})
