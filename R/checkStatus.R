#' Internal: Check the status of an existing LANDFIRE Product Service (LFPS) request
#'
#' @param landfire_api `landfire_api` object returned from `landfireAPIv2()`
#' @param verbose If FALSE suppress all status messages
#' @param method Passed to [utils::download.file()]. See `?download.file`
#' @param i Current iteration
#' @param max_time Maximum time, in seconds, to wait for job to be completed.
#'
#' @return Character vector (length = 1) with formated edit rules call
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' .checkStatus_internal(landfire_api, verbose = TRUE, method = "curl", i = 1, max_time = 10000)
#' }

.checkStatus_internal  <- function(landfire_api, verbose = TRUE, method = "curl",
                                   i, max_time) {

  # Check status
  response <- httr2::request(httr2::resp_url(landfire_api$request$request)) |>
    httr2::req_url_query("f"="pjson") |>
    httr2::req_perform()

  resp_body  <- jsonlite::fromJSON(httr2::resp_body_string(response))
  job_id <- resp_body$jobId

  # API always returns a successful status code (200) for LFPS v1
  status <- httr2::resp_status(response)

  # Parse content for messaging and error reporting
  job_status <- resp_body$jobStatus
  inf_msg  <- paste(resp_body$messages$type,
                    resp_body$message$description, sep = ": ")

  if (verbose == TRUE) {
    # There is a better way to do this but for now...clear console each loop:
    cat("\014")
    cat(job_status, "\nJob ID: ", job_id,
        "\nJob Messages:\n", paste(inf_msg, collapse = "\n"),
        "\n-------------------",
        "\nElapsed time: ", sprintf("%.1f", round(i*0.1, 1)), "s", "(Max time:", max_time, "s)",
        "\n-------------------\n")
  }

  # If failed exit and report
  if(status != 200 | grepl("Failed",job_status)) {
    warning(job_status)
    status <- "Failed"

    # If success report success and download file
  } else if (grepl("Succeeded",job_status)) {
    dwl_req <- httr2::request(response$url) |>
      httr2::req_url_path_append(resp_body$results$Output_File$paramUrl) |>
      # httr2::req_url_path_append("?f=pjson") |>
      httr2::req_perform()

    dwl_url <- jsonlite::fromJSON(httr2::resp_body_string(dwl_req))
    utils::download.file(dwl_url$value$url, landfire_api$path,
                         method = method, quiet = !verbose)
    status <- "Succeeded"
    landfire_api$request$dwl_url <- dwl_url$value$url
  }

  # Update landfire_api object
  landfire_api$request$date <- Sys.time()
  landfire_api$request$job_id <- job_id
  landfire_api$content <- inf_msg
  landfire_api$response <- response
  landfire_api$status <- status

  return(landfire_api)

}

#' Check the status of an existing LANDFIRE Product Service (LFPS) request
#'
#' @description
#' `checkStatus` checks if a previous request is complete and downloads available data
#'
#' @param landfire_api `landfire_api` object returned from `landfireAPIv2()`
#' @param verbose If FALSE suppress all status messages
#' @param method Passed to [utils::download.file()]. See `?download.file`
#'
#' @return
#' Returns a `landfire_api` object with named elements:
#' * `request` - list with elements `query`, `date`, `url`, `job_id`, `request`,`dwl_url`
#' * `content` - Informative messages passed from API
#' * `response` - Full response
#' * `status` - Final API status, one of "Failed", "Succeeded", or "Timed out"
#' * `path` - path to save directory
#'
#' @md
#' @export
#'
#' @examples
#' \dontrun{
#' products <-  c("ASP2020", "ELEV2020", "230CC")
#' aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
#' email <- "email@@example.com"
#' resp <- landfireAPIv2(products, aoi, email, background = TRUE)
#' checkStatus(resp)
#' }
checkStatus <- function(landfire_api, verbose = TRUE, method = "curl") {
  .checkStatus_internal(landfire_api, verbose = verbose, method = method,
                        i = 1, max_time = 0)
}

#' Cancel an active LANDFIRE Product Service (LFPS) API job
#'
#' @description
#' `cancelJob()` sends a request to cancel a LFPS API request
#'
#' @param job_id The job ID of the LFPS API request as a character string
#'
#' @return
#' Returns a `landfire_api` object with named elements:
#' * `request` - list with elements `query`, `date`, `url`, `job_id`,`dwl_url`
#' * `content` - Informative messages passed from API
#' * `response` - Full response
#' * `status` - Final API status, one of "Failed", "Succeeded", or "Timed out"
#' * `path` - path to save directory
#'
#' @md
#' @export
#'
#' @examples
#' \dontrun{
#' products <-  c("ASP2020", "ELEV2020", "230CC")
#' aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
#' email <- "email@@example.com>"
#'
#' resp <- landfireAPIv2(products, aoi, email)
#'
#' job_id <- resp$request$job_id #Get job_id from a previous request
#' cancelJob("job_id")
#' }

cancelJob <- function(job_id) {

  #### Checks
  # Missing
  stopifnot("argument `job_id` is missing with no default" = !missing(job_id))

  # Classes
  stopifnot("argument `job_id` must be a character string" = inherits(job_id, "character"))

  #### End Checks

  # Define Parameters
  params <- list(
    jobId = job_id
  )

  purpose <- "cancel"

  # Construct request URL
  request  <- httr2::request("https://lfps.usgs.gov/api/job/") |>
    httr2::req_url_path_append(purpose) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_user_agent("rlandfire (https://CRAN.R-project.org/package=rlandfire)")

  # Submit job
  response <- httr2::req_perform(request)

  # Construct landfire_api object
  .build_landfire_api(params = params, request = request, init_resp = response,
                      job_id = job_id, status = "Canceled")
}

#' Check if the LFPS API is available
#'
#' @description
#' `healthCheck()` checks if the LPFS API is available
#'
#' @return NULL. Prints a message to the console about the current status of LFPS.
#'
#' @md
#' @export
#'
#' @examples
#' \dontrun{
#' healthCheck()
#' }

healthCheck <- function() {

  # Construct request URL
  request  <- httr2::request("https://lfps.usgs.gov/api/healthCheck") |>
    httr2::req_user_agent("rlandfire (https://CRAN.R-project.org/package=rlandfire)")  |>
    httr2::req_headers("Accept" = "application/json")

  # Submit job
  response <- httr2::req_perform(request)

  # Parse content for messaging and error reporting
  resp_body  <- httr2::resp_body_json(response)

  if (resp_body$success) {
    message("The LFPS API is available")
  } else {
    warning("The LFPS API is currently down.\n",
            "Please notify the LANDFIRE helpdesk at <helpdesk@landfire.gov> of the following error:\n",
            resp_body$message)
  }
}
