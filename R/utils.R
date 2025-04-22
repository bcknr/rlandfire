#' View LFPS products table
#'
#' @description
#' `viewProducts()` opens the LFPS products table in your web browser
#'
#'
#' @return NULL. Opens the LF products table in your default browser.
#'
#' @md
#' @export
#'
#' @examples
#' \dontrun{
#' viewProducts()
#' }

viewProducts <- function() {
  utils::browseURL("https://lfps.usgs.gov/products")
}

#' Read in LANDFIRE products using the GDAL `virtual file system`
#'
#' @description
#' `landfire_vsi()` opens a request LANDFIRE GeoTIFF using the GDAL `virtual
#' file system` (VSI).
#'
#' @param landfire_api A `landfire_api` object created by `landfireAPIv2()`
#'
#' @details
#' The GDAL virtual file system allows you to read in LANDFIRE products without
#' having to download the file to your local machine within 60 minutes of the
#' request or if the file already exists on your local machine without having
#' to unzip it.
#'
#'
#' @return SpatRaster object of the requested LANDFIRE product/s
#'
#' @md
#' @export
#'
#' @examples
#' \dontrun{
#' aoi  <- c("-113.79", "42.148", "-113.56", "42.29")
#' email <- "email@example"
#' rast <- landfireAPIv2(products = "240EVC",
#'                       aoi = aoi, email = email,
#'                       method = "none")  |>
#'         landfireVSI()
#' }

landfireVSI <- function(landfire_api) {
  # checks
  if (!inherits(landfire_api, "landfire_api")) {
    stop("argument `landfire_api` must be a landfire_api object")
  }
  if (is.null(landfire_api$path) && is.null(landfire_api$request$dwl_url)) {
    stop("The provided `landfire_api` object does not contain a valid `path` or `dwl_url`")
  }
  # end checks

  if (!is.null(landfire_api$path) && file.exists(landfire_api$path)) {
    r <- terra::rast(
      sprintf("/vsizip/%s/%s",
              landfire_api$path,
              grep("\\.tif$",unzip(landfire_api$path, list=T)$Name,
                   value = TRUE))
    )
  } else if (!is.null(landfire_api$request$dwl_url)) {
    if (difftime(Sys.time(), landfire_api$time, units = "min") > 60) {
      stop("The requested file is no longer available for download")
    }

    r <- terra::rast(
      sprintf("/vsizip/vsicurl/%s/%s",
              landfire_api$request$dwl_url,
              gsub("\\.zip$",".tif", basename(landfire_api$request$dwl_url)))
    )
  } else {
    stop("No file associated with the provide `landfire_api` object was found")
  }

  return(r)
}


#' Internal: Build Landfire API object
#'
#' @param params List of parameters to be passed to the API
#' @param request httr2_request object
#' @param init_resp httr2_response object for the initial request
#' @param job_id Job ID
#' @param dwl_url URL to download the file
#' @param content Content of the response
#' @param response httr2_response object for the final request
#' @param status Status of the request
#' @param path Path to the file
#'
#' @return Character vector (length = 1) with formated edit rules call
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' 
#' }
.build_landfire_api <- function(params = NULL, request = NULL, init_resp = NULL,
                                job_id = NULL, dwl_url = NULL, content = NULL,
                                response = NULL, status = NULL, time = NULL,
                                path = NULL) {
  structure(
    list(
      request = list(query = params, # User input in rlandfire formats
                     date = Sys.time(),
                     url = if (!is.null(request)) request$url else NULL,
                     job_id = job_id,
                     request = init_resp,
                     dwl_url = dwl_url),
      content = content,
      response = response,
      status = status,
      time = time,
      path = path
    ),
    class = "landfire_api"
  )
}