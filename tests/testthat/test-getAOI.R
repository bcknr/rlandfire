test_that("`getAOI` works with supported object classes", {
  r <- terra::rast(nrows = 50, ncols = 50,
                   xmin = -30, xmax = 10,
                   ymin = -30, ymax = 10,
                   crs = terra::crs("epsg:4326"),
                   vals = rnorm(2500))
  v <- terra::as.polygons(r)
  ext <- c("xmin" = -30, "xmax" = 10, "ymin" = -30, "ymax" = 10)

  expect_equal(getAOI(r), ext)
  expect_equal(getAOI(v), ext)
  expect_equal(getAOI(sf::st_as_sf(v)), ext)
  expect_equal(getAOI(raster::raster(r)), ext)
  expect_equal(getAOI(stars::st_as_stars(r)), ext)

})
