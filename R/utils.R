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
#' viewProducts()
#'

viewProducts <- function() {
  utils::browseURL("https://lfps.usgs.gov/helpdocs/productstable.html")
}
