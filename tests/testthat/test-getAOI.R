test_that("`getAOI` works with supported object classes", {
  r <- terra::rast(nrows = 50, ncols = 50,
                   xmin = -123.7835, xmax = -123.6352,
                   ymin = 41.7534, ymax = 41.8042,
                   crs = terra::crs("epsg:4326"),
                   vals = rnorm(2500))
  v <- terra::as.polygons(r)
  ext <- c(-123.7835, 41.7534, -123.6352, 41.8042)

  expect_equal(getAOI(r), ext)
  expect_equal(getAOI(v), ext)
  expect_equal(getAOI(sf::st_as_sf(v)), ext)
  expect_equal(getAOI(raster::raster(r)), ext)
  expect_equal(getAOI(stars::st_as_stars(r)), ext)

})
