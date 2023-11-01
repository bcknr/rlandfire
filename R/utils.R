#' View LFPS products table
#'
#' @description
#' `viewProducts()` opens the LFPS products table in your web browser
#'
#'
#' @return NULL
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
