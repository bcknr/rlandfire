# Tests for landfireAPI.R

test_that("`landfireAPI()` recognizes arguement errors", {
  products <-  c("ASP2020", "ELEV2020", "140CC")
  aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
  projection <- 6414
  resolution <- 90
  edit_rule <- list(c("condition","ELEV2020","lt",500), c("change", "140CC", "st", 181))
  path <- tempfile(fileext = ".zip")

  # Check for required arguements
  expect_error(landfireAPI(aoi = aoi),
               "argument `products` is missing with no default")

  expect_error(landfireAPI(products),
               "argument `aoi` is missing with no default")

  # Check class
  expect_error(landfireAPI(products = c(1,2,3), aoi, path = path),
               "argument `products` must be a character vector")
  expect_error(landfireAPI(products, aoi = list(1,2,3), path = path),
               "argument `aoi` must be a character or numeric vector")
  expect_error(landfireAPI(products, aoi, max_time = TRUE, path = path),
               "argument `max_time` must be numeric")
  expect_error(landfireAPI(products, aoi, verbose = "yes", path = path),
               "argument `verbose` must be logical")
  expect_error(landfireAPI(products, aoi, edit_rule = "edit_rule"),
               "argument `edit_rule` must be a list")

  # Check edit_rule arguements
  expect_error(landfireAPI(products, aoi,
                           edit_rule = list(c("wrong","ELEV2020","lt",500), c("change", "140CC", "st", 181))),
               '`edit_rule` operator classes must only be "condition" or "change"')
  expect_error(landfireAPI(products, aoi,
                           edit_rule = list(c("condition","ELEV2020","xx",500), c("change", "140CC", "st", 181))),
               '`edit_rule` conditional operators must be one of "eq","ge","gt","le","lt","ne"')
})


test_that("`landfireAPI()` recognizes failed call", {
  products <-  "NotAProduct"
  aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
  projection <- 123456
  path <- tempfile(fileext = ".zip")

  expect_error(landfireAPI(products, aoi, projection, path = path),
               "Job Status:  esriJobFailed")
})


# Tests for .fmt_editrules (internal)
