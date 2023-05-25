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

  # Check edit_rule arguments
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

  expect_warning(landfireAPI(products, aoi, projection, path = path),
               "Job Status:  esriJobFailed")
})


# Tests for .fmt_editrules (internal)

test_that("`.fmt_editrules` correctly formats requests",{
  single_rule <- list(c("condition","ELEV2020","lt",500), c("change", "140CC", "st", 181))
  multi_rule <- list(c("condition","ELEV2020","lt",500), c("change", "140CC", "st", 181),
                    c("condition","ELEV2020","ge",600), c("change", "140CC", "db", 20),
                    c("condition","ELEV2020","eq",550), c("change", "140CC", "st", 0))

  expect_identical(.fmt_editrules(single_rule),
                   "{\"edit\":[{\"condition\":[{\"product\":\"ELEV2020\",\"operator\":\"lt\",\"value\":500}],\"change\":[{\"product\":\"140CC\",\"operator\":\"st\",\"value\":181}]}]}")
  expect_identical(.fmt_editrules(multi_rule),
                   "{\"edit\":[{\"condition\":[{\"product\":\"ELEV2020\",\"operator\":\"lt\",\"value\":500}],\"change\":[{\"product\":\"140CC\",\"operator\":\"st\",\"value\":181}],\"condition\":[{\"product\":\"ELEV2020\",\"operator\":\"ge\",\"value\":600}],\"change\":[{\"product\":\"140CC\",\"operator\":\"db\",\"value\":20}],\"condition\":[{\"product\":\"ELEV2020\",\"operator\":\"eq\",\"value\":550}],\"change\":[{\"product\":\"140CC\",\"operator\":\"st\",\"value\":0}]}]}")
})
