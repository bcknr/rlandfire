#' Call the LANDFIRE Product Service (LFPS) API
#'
#' @description
#' `landfireAPIv2` downloads LANDFIRE data by calling the LFPS API
#'
#' @param products Product names as character vector
#'   (see: \href{https://lfps.usgs.gov/products}{Products Table})
#' @param aoi Area of interest as character or numeric vector defined by
#'   latitude and longitude in decimal degrees in WGS84 and ordered
#'   `xmin`, `ymin`, `xmax`, `ymax` or a LANDFIRE map zone.
#' @param email Email address as character string. This is a required argument
#'   for the LFPS v2 API. See the \href{https://lfps.usgs.gov/LFProductsServiceUserGuide.pdf}{LFPS Guide} 
#'   for more information. Outside of the LFPS API request, this email address
#'   is not used for any other purpose, stored, or shared by `rlandfire`.
#' @param projection Optional. A numeric value of the WKID for the output projection
#'    Default is a localized Albers projection.
#' @param resolution Optional. A numeric value between 30-9999 specifying the
#'   resample resolution in meters. Default is 30m.
#' @param edit_rule Optional. A list of character vectors ordered "operator class"
#'   "product", "operator", "value" where "operator class" is one of "condition",
#'   "ORcondition", or "change". Edits are limited to fuel theme products only.
#'   (see: \href{https://lfps.usgs.gov/LFProductsServiceUserGuide.pdf}{LFPS Guide})
#' @param edit_mask Optional. Path to a compressed shapefile (.zip) to be used
#'   as an edit mask. The shapefile must be less than 1MB in size and must 
#'   comply with ESRI shapefile naming rules.
#' @param priority_code Optional. Priority code for wildland fire systems/users.
#'   Contact the LANDFIRE help desk for information (<helpdesk@landfire.gov>)
#' @param path Path to `.zip` directory. Passed to [utils::download.file()].
#'   If NULL, a temporary directory is created.
#' @param max_time Maximum time, in seconds, to wait for job to be completed.
#' @param method Passed to [utils::download.file()]. See `?download.file`
#' @param verbose If FALSE suppress all status messages
#' @param background If TRUE, the function will return immediately and the job
#'   will run in the background. User will need to check the status of the job
#'   manually with `checkStatus()`.
#'
#' @return
#' Returns a `landfire_api` object with named elements:
#' * `request` - list with elements `query`, `date`, `url`, `job_id`, `request`,`dwl_url`
#' * `content` - Informative messages passed from API
#' * `response` - Full response
#' * `status` - Final API status, one of "Failed", "Succeeded", or "Timed out"
#' * `time` - time of job completion
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
#' projection <- 6414
#' resolution <- 90
#' edit_rule <- list(c("condition","ELEV2020","lt",500),
#'                   c("change", "230CC", "st", 181))
#' save_file <- tempfile(fileext = ".zip")
#' resp <- landfireAPIv2(products, aoi, email, projection,
#'                       resolution, edit_rule = edit_rule,
#'                       path = save_file)
#' }

landfireAPIv2 <- function(products, aoi, email, projection = NULL,
                        resolution = NULL, edit_rule = NULL, edit_mask = NULL,
                        priority_code = NULL, path = NULL, max_time = 10000,
                        method = "curl", verbose = TRUE, background = FALSE) {

  #### Checks
  # Missing
  stopifnot("argument `products` is missing with no default" = !missing(products))
  stopifnot("argument `aoi` is missing with no default" = !missing(aoi))
  stopifnot("A valid `email` address is required. (See `?rlandfire::landfireAPIv2` for more information)"
            = grepl("@", email) && !missing(email))
  stopifnot("argument `email` is missing with no default" = !missing(aoi))

  if(!is.null(edit_rule)){
    stopifnot("argument `edit_rule` must be a list"
              = inherits(edit_rule, "list"))

    class <- sapply(edit_rule, `[`, 1)

    stopifnot(
      '`edit_rule` operator classes must only be "condition" or "change"' =
        all(class %in% c("condition", "change", "ORcondition"))
    )

    stopifnot(
      '`edit_rule` conditional operators must be one of "eq","ge","gt","le","lt","ne"'
      = all(sapply(edit_rule, `[`, 3)[class %in% c("condition","ORcondition")] %in% c("eq","ge","gt","le","lt","ne"))
    )

    stopifnot(
      '`edit_rule` change operators must be one of "cm","cv","cx","bd","ib","mb","st"'
      = all(sapply(edit_rule, `[`, 3)[class == "change"] %in% c("cm","cv","cx","db","ib","mb","st"))
    )

    stopifnot(
      "all products used in argument `edit_rule` must be included in argument `products`"
      = all(sapply(edit_rule, `[`, 2) %in% products)
    )
  }

  if(is.null(path) && method != "none") {
    path = tempfile(fileext = ".zip")
    message("`path` is missing. Files will be saved in temporary directory: ", 
            path)
  }

  if (!is.null(edit_mask) && is.null(edit_rule)) {
    stop("`edit_mask` requires `edit_rule` to be specified.")
  }

  # Classes
  stopifnot("argument `products` must be a character vector"
            = inherits(products, "character"))
  stopifnot("argument `aoi` must be a character or numeric vector"
            = inherits(aoi, c("character", "numeric")))
  stopifnot("argument `aoi` must be vector of coordinates with length == 4 or a single map zone"
            = length(aoi) == 1 | length(aoi) == 4)
  stopifnot("argument `max_time` must be numeric"
            = inherits(max_time, c("numeric")))
  stopifnot("argument `priority_code` must be a character string"
            = inherits(priority_code, c("character", "NULL")))
  stopifnot("argument `edit_mask` must be a character string"
            = inherits(edit_mask, c("character", "NULL")))
  stopifnot("argument `background` must be a logical"
            = inherits(background, "logical"))

  stopifnot(
    "`method` is invalid. See `?download.file` or use \"none\" to skip download"
    = method %in% c("internal", "libcurl", "wget",
                    "curl", "wininet", "auto", "none")
  )

  stopifnot("argument `verbose` must be logical" = inherits(verbose, "logical"))

  # stopifnot("argument `max_time` must be >= 5" = max_time >= 5)

  # Values in range
  if(is.numeric(resolution) && resolution == 30) {
    resolution <- NULL
  }

  if(!is.null(resolution) && !all(resolution >= 30 & resolution <= 9999)) {
    stop("argument `resolution` must be between 30 and 9999 or `NULL`")
  }

  aoi <- as.numeric(aoi)

  if(length(aoi) == 4) {
    # Likely lat/lon?
    if(!all(aoi[c(1,3)] >=-180 & aoi[c(1,3)] <=180 &
            aoi[c(2,4)] >=-90 & aoi[c(2,4)] <=90)){
      stop("argument `aoi` must be latitude and longitude in decimal degrees (WGS84) or a LANDFIRE map zone")
    }
    # Correct order?
    if(!all(aoi[[1]] < aoi[[3]] & aoi[[2]] < aoi[[4]])) {
      stop("argument `aoi` must be ordered `xmin`, `ymin`, `xmax`, `ymax`")
    }
    # Valid map zone?
  } else if(length(aoi) == 1 && !all(aoi >= 1 & aoi <= 79)){
    stop("argument `aoi` must be between 1 and 79 if using LANDFIRE map zones")
  }

  #### End Checks

  # Edit mask
  mask <- .post_editmask(edit_mask)

  # Define Parameters
  params <- list(
    Email = email,
    Layer_List = paste(products, collapse = ";"),
    Area_of_Interest = paste(aoi, collapse = " "),
    Output_Projection = projection,
    Resample_Resolution = resolution,
    Edit_Rule = .fmt_editrules(edit_rule, mask = mask),
    Edit_Mask = mask$item_id,
    Priority_Code = priority_code
  )

  purpose <- "submit"

  # Construct request URL
  request  <- httr2::request("https://lfps.usgs.gov/api/job/") |>
    httr2::req_url_path_append(purpose) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_user_agent("rlandfire (https://CRAN.R-project.org/package=rlandfire)") |>
    httr2::req_headers("Accept" = "application/json")

  # Submit job and get initial response
  req <- httr2::req_error(request, is_error = \(req) FALSE) |>
         httr2::req_perform()

  req_response <- httr2::resp_body_json(req, simplifyVector = TRUE)

  if(req$status != 200) {
    stop("\tAPI request failed with status code: ", req$status,
         "\n\tLFPS Error message: ", req_response$message)
  }

  lfps <- .build_landfire_api(params = params, request = request,
                              job_id = req_response$jobId,
                              init_resp = req, path = path)

  mt <- max_time*10

  for (i in 1:mt) {
    # Check status
    lfps_return <- .checkStatus_internal(landfire_api = lfps, verbose = verbose,
                                method = method, i = i, max_time = max_time)

    # Set early exit if background is TRUE or status is "Failed" or "Succeeded"
    if (background == TRUE || max_time == 0) {
      message("Job submitted in background.\n",
              "Call `checkStatus()` to check the current status and download",
              " if completed.\n",
              "Or visit URL to check status and download manually:\n   ",
              lfps_return$response$url)
      break
    } else if (lfps_return$status %in% c("Failed",
                                         "Succeeded",
                                         "Succeeded (download skipped)")) {
      break
    }

    # Max time error
    if(i == mt) {
      cat("\n")
      warning("Job status: Incomplete and `max_time` reached\n",
              "Call: `checkStatus()` to check the current status\n",
              "      `cancelJob()` to cancel.\n",
              "Or visit URL to check status and download manually:\n   ",
              lfps_return$response$url)
      lfps_return$status <- "Timed out"
      break
    } else {
      Sys.sleep(0.1)
    }
  }

  return(lfps_return)

}

#' Internal: Submit edit_mask POST request
#'
#' @param file Path to the zipped shape file to be uploaded
#'
#' @return JSON array returned by POST request
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' edit_mask <- system.file("extdata", "wildfire.zip", package = "rlandfire")
#' .post_editmask(edit_mask)
#' }
.post_editmask <- function(file) {

  if(is.null(file)) {
    return(list(item_id = NULL,
                item_name = NULL))
  }

  # Checks
  stopifnot("`edit_mask` file not found" = file.exists(file))
  stopifnot("`edit_mask` file exceeds maximum allowable size (1MB)"
            = file.info(file)$size < 1000000)
  stopifnot("`edit_mask` file must be a zipped shapefile (.zip)"
            = grepl("\\.zip$", file))
  stopifnot("`edit_mask` file does not contain a shapefile"
            = any(grepl("\\.shp$", unzip(file, list=T)$Name)))

  stopifnot("`edit_mask` file name must not contain special characters"
            = !grepl("[^[:alnum:].]", basename(file)))
  # End Checks

  req <- httr2::request("https://lfps.usgs.gov/api/upload/shapefile") |>
    httr2::req_method("POST") |>
    httr2::req_user_agent("rlandfire (https://CRAN.R-project.org/package=rlandfire)") |>
    httr2::req_headers("Accept" = "application/json") |>
    httr2::req_body_multipart(
      file = curl::form_file(file, type = "application/zip"),
      description = "string"
    )

  # Perform the request
  upload_resp <- httr2::req_error(req, is_error = \(upload_resp) FALSE) |>
                 httr2::req_perform()

  upload_body <- httr2::resp_body_json(upload_resp)

  # Check for errors
  if (upload_resp$status != 200) {
    stop("\t`edit_mask` upload failed with status code: ", upload_resp$status,
         "\n\tLFPS Error message: ", upload_body$message)
  }

  return(list(item_id = sprintf('[{"itemID":"%s"}]', upload_body$itemId),
              item_name = grep("\\.shp$",
                               unzip(file, list=T)$Name,
                               value = TRUE)))

}


#' Internal: Format edit_rule for API call
#'
#' @param rules A list of character vectors ordered "operator class"
#'   "product", "operator", "value". Limited to fuel theme products only.
#'
#' @return Character vector (length = 1) with formated edit rules call
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' edit_rule <- list(c("condition","ELEV2020","lt",500),
#'                   c("change", "230CC", "st", 181))
#' .fmt_editrules(edit_rule)
#' }
.fmt_editrules <- function(rules, mask = NULL) {

  # Check for NULL
  if (is.null(rules)) {
    return(NULL)
  }

  class <- sapply(rules, `[`, 1)

  params <- lapply(rules, function(x) {
    paste0('"product":"', x[2],
           '","operator":"', x[3],
           '","value":', x[4])
  })

  # Condition - ID groups with same class and build request
  cnd <- grep(".*condition", class)
  breaks <- c(0, which(diff(cnd) != 1), length(cnd))
  cnd_grp <- lapply(seq(length(breaks) - 1),
                    function(i) cnd[(breaks[i] + 1):breaks[i+1]])

  cnd <- lapply(cnd_grp, function(i) paste0('"condition":[{',
                                            paste0(params[i],
                                            collapse = '},{'),'}]'))

  # Check for OR condition
  or_cnd <- grep("OR.*", class)
  stopifnot("`edit_rule` contains a `condition` without an associated `change`."
            = or_cnd %in% sapply(cnd_grp, `[`, 1))

  # Add edit mask if provided
  if(!is.null(mask)) {

    mask_file  <- sprintf('"mask":"%s"', mask$item_name)

    # mask groups
    mask_grp  <- c(1, which(do.call("c", cnd_grp) %in% or_cnd))

    if (length(mask$item_name) == 1 ||
        length(mask$item_name) == length(cnd_grp)) {
      cnd[mask_grp]  <- paste(mask_file, cnd[mask_grp], sep = ",")
    } else {
      stop("The number of `edit_mask` count should match the number of", 
           "`edit_rules` condition groups when using multiple shapefiles.")
    }
  }

  # Change - ID groups with same class and build request
  chng <- which(class == "change")
  breaks <- c(0, which(diff(chng) != 1), length(chng))
  chng_grp <- lapply(seq(length(breaks) - 1),
                     function(i) chng[(breaks[i] + 1):breaks[i+1]])

  chng <- lapply(chng_grp, function(i) paste0('"change":[{',
                                              paste0(params[i],
                                              collapse = '},{'),'}]'))

  # Retain original order
  order_cnd <- sapply(cnd_grp, `[`, 1)
  order_chng <- sapply(chng_grp, `[`, 1)

  edit_rule <- c()
  edit_rule[order_cnd] <- unlist(cnd)
  edit_rule[order_chng] <- unlist(chng)

  # Check if multiple "Edit" groups needed
  ed_grp <- edit_rule[c(1, or_cnd)]
  out_rules <- edit_rule[!is.na(edit_rule)]

  # If OR condition
  if (length(ed_grp) > 1) {
    out_rules <- lapply(which(out_rules %in% ed_grp), function(i) {
    sprintf('"edit":[{%s,%s}]', out_rules[i], out_rules[i + 1])
  })
  } else {
    out_rules <- paste0('"edit":[{', 
                       paste(out_rules, collapse = ','),
                        '}]')
  }

  # Assemble final string
  sprintf("{%s}",
    paste0(out_rules[!is.na(out_rules)], collapse = ",")
  )
}

# Depreciated -----------------------------------------------------------


#' Depreciated: Call the LANDFIRE Product Service (LFPS) API
#'
#' @description
#' Depreciated: `landfireAPI()` is no longer supported due to updates to the 
#' LFPS API. Use `landfireAPIv2()` instead.
#' 
#' `landfireAPI` downloads LANDFIRE data by calling the LFPS API
#'
#' @param products Product names as character vector
#'   (see: Products Table)
#' @param aoi Area of interest as character or numeric vector defined by
#'   latitude and longitude in decimal degrees in WGS84 and ordered
#'   `xmin`, `ymin`, `xmax`, `ymax` or a LANDFIRE map zone.
#' @param projection Optional. A numeric value of the WKID for the output projection
#'    Default is a localized Albers projection.
#' @param resolution Optional. A numeric value between 30-9999 specifying the
#'   resample resolution in meters. Default is 30m.
#' @param edit_rule Optional. A list of character vectors ordered "operator class"
#'   "product", "operator", "value". Limited to fuel theme products only.
#'   (see: LFPS Guide)
#' @param edit_mask Optional. **Not currently functional**
#' @param path Path to `.zip` directory. Passed to [utils::download.file()].
#'   If NULL, a temporary directory is created.
#' @param max_time Maximum time, in seconds, to wait for job to be completed.
#' @param method Passed to [utils::download.file()]. See `?download.file` or
#'   use "none" to skip download and use `landfire_vsi()`
#' @param verbose If FALSE suppress all status messages
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
#' projection <- 6414
#' resolution <- 90
#' edit_rule <- list(c("condition","ELEV2020","lt",500), c("change", "230CC", "st", 181))
#' save_file <- tempfile(fileext = ".zip")
#' resp <- landfireAPI(products, aoi, projection, resolution, edit_rule = edit_rule, path = save_file)
#' }

landfireAPI <- function(products, aoi, projection = NULL, resolution = NULL,
                        edit_rule = NULL, edit_mask = NULL, path = NULL,
                        max_time = 10000, method = "curl", verbose = TRUE) {
  requireNamespace("lifecycle", quietly = TRUE)
  lifecycle::deprecate_warn(
    "1.0.0",
    "landfireAPI()",
    "landfireAPIv2()",
    c("The LANDFIRE Products Services API has been updated to v2 as of May 2025.",
      "New parameters and syntax are required.",
      "See `?rlandfire::landfireAPIv2` for more information.")
  )

  #### Checks
  # Missing
  stopifnot("argument `products` is missing with no default" = !missing(products))
  stopifnot("argument `aoi` is missing with no default" = !missing(aoi))

  if(!is.null(edit_rule)){
    stopifnot("`edit_rule` must be a list" = inherits(edit_rule, "list"))

    class <- sapply(edit_rule, `[`, 1)

    stopifnot(
      '`edit_rule` operator classes must only be "condition" or "change"' =
        all(class %in% c("condition", "change"))
    )

    stopifnot(
      '`edit_rule` conditional operators must be one of "eq","ge","gt","le","lt","ne"' =
        all(sapply(edit_rule, `[`, 3)[class == "condition"] %in% c("eq","ge","gt","le","lt","ne")))

    stopifnot(
      '`edit_rule` change operators must be one of "cm","cv","cx","bd","ib","mb","st"' =
        all(sapply(edit_rule, `[`, 3)[class == "change"] %in% c("cm","cv","cx","db","ib","mb","st")))
  }

  if(is.null(path)){
    path = tempfile(fileext = ".zip")
    warning("`path` is missing. Files will be saved in temp directory: ", path)
  }

  if (method == "none") {
    path <- NULL
  }

  # Classes
  stopifnot("argument `products` must be a character vector" = inherits(products, "character"))
  stopifnot("argument `aoi` must be a character or numeric vector" = inherits(aoi, c("character", "numeric")))
  stopifnot("argument `aoi` must be vector of coordinates with length == 4 or a single map zone" = length(aoi) == 1 | length(aoi) == 4)
  stopifnot("argument `max_time` must be numeric" = inherits(max_time, c("numeric")))

  stopifnot(
    "`method` is invalid. See `?download.file`" =
      method %in% c("internal", "libcurl", "wget", "curl", "wininet", "auto")
  )

  stopifnot("argument `verbose` must be logical" = inherits(verbose, "logical"))

  stopifnot("argument `max_time` must be >= 5" = max_time >= 5)

  #check for special characters in edit mask
  # grepl('[^[:alnum:]]', val)
  # Check it is sf or spatvector

  # Values in range
  if(is.numeric(resolution) && resolution == 30) {
    resolution <- NULL
  }

  if(!is.null(resolution) && !all(resolution >= 30 & resolution <= 9999)) {
    stop("argument `resolution` must be between 30 and 9999 or `NULL`")
  }

  aoi <- as.numeric(aoi)

  if(length(aoi) == 4) {
    # Likely lat/lon?
    if(!all(aoi[c(1,3)] >=-180 & aoi[c(1,3)] <=180 &
            aoi[c(2,4)] >=-90 & aoi[c(2,4)] <=90)){
      stop("argument `aoi` must be latitude and longitude in decimal degrees (WGS84) or a LANDFIRE map zone")
    }
    # Correct order?
    if(!all(aoi[[1]] < aoi[[3]] & aoi[[2]] < aoi[[4]])) {
      stop("argument `aoi` must be ordered `xmin`, `ymin`, `xmax`, `ymax`")
    }
    # Valid map zone?
  } else if(length(aoi) == 1 & !all(aoi >= 1 & aoi <= 79)){
    stop("argument `aoi` must be between 1 and 79 when using LANDFIRE map zones")
  }

  #### End Checks

  if(!is.null(edit_rule)) {
    edit_rule <- .fmt_editrules(edit_rule)
  }

  base_url <- httr::parse_url("https://lfps.usgs.gov/arcgis/rest/services/LandfireProductService/GPServer/LandfireProductService/submitJob?")
  base_url$query <- list(Layer_List = paste(products, collapse = ";"),
                         Area_of_Interest = paste(aoi, collapse = " "),
                         Output_Projection = projection,
                         Resample_Resolution = resolution,
                         Edit_Rule = edit_rule #,
                         #Edit_Mask = edit_mask
  )

  url <- httr::build_url(base_url)
  r <- httr::GET(url)
  job_id <- sub(".*jobs/(.*)$", "\\1", r$url)
  dwl_url <- paste0("https://lfps.usgs.gov/arcgis/rest/directories/arcgisjobs/landfireproductservice_gpserver/",
                    job_id, "/scratch/", job_id, ".zip")

  # Loop through up to max time
  mt <- max_time*10

  for (i in 1:mt) {
    # Check status
    r <- httr::GET(r$url)
    status <- httr::status_code(r) #API always returns a successful status code (200)
    content <- strsplit(httr::content(r, "text"), "\r\n")[[1]]

    # Parse content for messaging and error reporting
    message <- gsub("\\<.*?\\>", "", content, perl = TRUE)
    job_status <- message[grep("Job Status", message)]
    inf_msg <- message[grep("esriJobMessageType", message)]

    if(verbose == TRUE) {
      # There is a better way to do this but for now...clear console each loop:
      cat("\014")

      cat(job_status,"\nJob Messages:\n",paste(inf_msg, collapse = "\n"),
          "\n-------------------",
          "\nElapsed time: ", sprintf("%.1f", round(i*0.1, 1)), "s", "(Max time:", max_time, "s)",
          "\n-------------------\n")
    }

    # If failed exit and report
    if(status != 200 | grepl("Failed",job_status)) {
      warning(job_status)
      status <- "Failed"
      break

      # If success report success and download file
    } else if(grepl("Succeeded",job_status)) {
      utils::download.file(dwl_url, path, method = method, quiet = !verbose)
      status <- "Succeeded"
      break

      # Print current status, wait, and check again
    } else {
      Sys.sleep(0.1)
    }

    # Max time error
    if(i == mt) {
      cat("\n")
      warning("Job status: Incomplete and max_time reached\nVisit URL to check status and download manually:\n   ", r$url)
      status <- "Timed out"
      break
    }

  }

  # construct landfire_api object
  structure(
    list(
      request = list(query = base_url$query,
                     date = Sys.time(),
                     url = url,
                     job_id = job_id,
                     dwl_url = dwl_url),
      content = inf_msg, # currently just returns a vector
      response = r,
      status = status,
      path = path
    ),
    class = "landfire_api"
  )

}