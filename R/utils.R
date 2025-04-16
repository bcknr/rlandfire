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
  utils::browseURL("https://lfps.usgs.gov/lfps/helpdocs/productstable.html")
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
                                response = NULL, status = NULL, path = NULL) {
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
      path = path
    ),
    class = "landfire_api"
  )
}