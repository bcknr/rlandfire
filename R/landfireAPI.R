#' Call the LANDFIRE Product Service (LFPS) API
#'
#' @description
#' `landfireAPI` downloads LANDFIRE data by calling the LFPS API
#'
#' @param products Product names as character vector
#'   (see: https://lfps.usgs.gov/helpdocs/productstable.html)
#' @param aoi Area of interest as character or numeric vector defined by
#'   latitude and longitude in decimal degrees in WGS84 and ordered
#'   `xmin`, `ymin`, `xmax`, `ymax` or a LANDFIRE map zone.
#' @param projection Optional. A numeric value of the WKID for the output projection
#'    Default is a localized Albers projection.
#' @param resolution Optional. A numeric value between 31-9999 specifying the
#'   resample resolution in meters. Default is 30m.
#' @param edit_rule Optional. **Not currently functional**
#' @param edit_mask Optional. **Not currently functional**
#' @param path Path to `.zip` directory. Passed to `utils::download.file()`.
#'   If NULL, a temporary directory is created.
#' @param max_time Maximum time, in seconds, to wait for job to be completed.
#' @param method Passed to `utils::download.file()`. See `?download.file`
#' @param verbose If FALSE suppress all status messages
#'
#' @return Returns API call passed from httr::get(). Downloads files to `path`
#' @export
#'
#' @examples
#' \dontrun{
#' products <-  c("ASP2020", "ELEV2020", "SLPP2020")
#' aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
#' projection <- 6414
#' resolution <- 90
#' save_file <- tempfile(fileext = ".zip")
#' test <- landfireAPI(products, aoi, projection, resolution, path = save_file)
#' }

landfireAPI <- function(products, aoi, projection = NULL, resolution = NULL,
                        edit_rule = NULL, edit_mask = NULL, path = NULL,
                        max_time = 1000, method = "curl", verbose = TRUE) {

  #### Checks
  # Missing
  stopifnot("argument `products` is missing with no default" != !missing(products))
  stopifnot("argument `aoi` is missing with no default" != !missing(aoi))

  if(is.null(path)){
    path = tempfile(fileext = ".zip")
    warning("`path` is missing. Files will be saved in temp directory:", path)
  }

  # Classes
  stopifnot("argument `products` must be a character vector" = inherits(products, "character"))
  stopifnot("argument `aoi` must be a character or numeric vector" = inherits(aoi, c("character", "numeric")))
  stopifnot("argument `max_time` must be numeric" = inherits(max_time, c("numeric")))

  stopifnot(
    "`method` is invalid. See `?download.file`" =
      method %in% c("internal", "libcurl", "wget", "curl", "wininet", "auto")
  )

  stopifnot("argument `verbose` must be logical" = inherits(verbose, "logical"))

  #### End Checks

  base_url <- httr::parse_url("https://lfps.usgs.gov/arcgis/rest/services/LandfireProductService/GPServer/LandfireProductService/submitJob?")
  base_url$query <- list(Layer_List = paste(products, collapse = ";"),
                         Area_of_Interest = paste(aoi, collapse = " "),
                         Output_Projection = projection,
                         Resample_Resolution = resolution #,
                         #Edit_Rule = edit_rule,
                         #Edit_Mask = edit_mask
  )

  url <- httr::build_url(base_url)
  r <- httr::GET(url)
  job_id <- stringr::str_extract(r$url, ".{33}$") #NOTE: Assumes that job id length is always 33 characters
  dwl_url <- paste0("https://lfps.usgs.gov/arcgis/rest/directories/arcgisjobs/landfireproductservice_gpserver/",
                    job_id, "/scratch/", job_id, ".zip")

  # Loop through up to max time
  mt <- max_time/5

  for (i in 1:mt) {
    # Check status
    r <- httr::GET(r$url)
    status <- httr::status_code(r) #API always returns a successful status code (200)
    content <- strsplit(httr::content(r, "text"), "\r\n")[[1]]

    # Parse content for messaging and error reporting
    message <- stringr::str_replace_all(content, "\\<.*?\\>", "")
    job_status <- message[grep("Job Status", message)]
    inf_msg <- message[grep("esriJobMessageType", message)]

    if(verbose == TRUE) {
      # There is a better way to do this but for now...
      cat("\014") # clear console each loop

      cat(job_status,"\nJob Messages:\n",paste(inf_msg, collapse = "\n"),
          "\n-------------------",
          "\nElapsed time: ", i * 0.1, "s", "(Max time:", max_time, "s)",
          "\n-------------------\n")
    }

    # If failed exit and report
    if(status != 200 | grepl("Failed",job_status)) {
      stop(job_status)
      break

      # If success report success and download file
    } else if(grepl("Succeeded",job_status)) {
      utils::download.file(dwl_url, path, method = method, quiet = !verbose)
      break

      # Print current status, wait, and check again
    } else {
      Sys.sleep(0.1)
    }

    # Max time error
    if(i == mt) {
      cat("\n")
      stop("Job status: Incomplete and max_time reached\nVisit URL to check status and download manually\n   ", r$url)
    }

  }

  return(r)

}

#TODO Get edit_rule and edit_mask running
#TODO Overwrite the console instead of clearing
