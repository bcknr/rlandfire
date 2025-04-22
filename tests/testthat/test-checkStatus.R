# Test for checkStatus.R

# Tests for .checkStatus_internal()
httptest2::with_mock_dir("_mock/checkStatus-nojob", {
  test_that("`.checkStatus_internal()` recognizes API errors", {
    noJob_landfire_api <- .build_landfire_api(job_id = "123456")
    expect_error(.checkStatus_internal(noJob_landfire_api, i = 1, max_time = 10),
                "\tAPI request failed with status code: 500\n\tLFPS Error message: JobId not found")
  })
})

httptest2::with_mock_dir("_mock/checkStatus", {
  test_that("`.checkStatus_internal()` returns expected", {
    landfire_api <- .build_landfire_api(job_id = "be7fab41-867e-41a5-b594-c469f118bc49",
                                        path = tempfile(fileext = ".zip"))
    return <- .checkStatus_internal(landfire_api, i = 1, max_time = 10)
    expect_s3_class(return, "landfire_api")
    expect_equal(return$request$job_id, "be7fab41-867e-41a5-b594-c469f118bc49")
  })
})

# Tests for checkStatus()
test_that("`checkStatus()` recognizes argument errors", {
  expect_error(checkStatus(),
               "argument `landfire_api` is missing with no default",
               fixed = TRUE)

  expect_error(checkStatus(landfire_api = list()),
               "argument `landfire_api` must be a landfire_api object")
  
  expect_error(checkStatus(landfire_api = .build_landfire_api(),
                           verbose = "not logical"),
               "argument `verbose` must be logical")
  
  expect_error(checkStatus(landfire_api = .build_landfire_api(),
                           method = "not a method"),
               "`method` is invalid. See `?download.file`",
               fixed = TRUE)
})

# Tests for cancelJob()
test_that("`cancelJob()` recognizes arguement errors", {
  expect_error(cancelJob(),
               "argument `job_id` is missing with no default")

  expect_error(cancelJob(job_id = 123456),
               "argument `job_id` must be a character string")
})

# Check functionality of `cancelJob()`
httptest2::with_mock_dir("_mock/cancelJob", {
  test_that("cancelJob() returns expected response", {

    # Check class and values
    expect_null(cancelJob("6f5ba39c-09f7-4f6d-97fa-543af185eb1b"))

    # Check message
    expect_message(cancelJob("6f5ba39c-09f7-4f6d-97fa-543af185eb1b"),
    "Job 6f5ba39c-09f7-4f6d-97fa-543af185eb1b has been canceled")
  })
})

# Check functionality of `healthCheck()`
httptest2::with_mock_dir("_mock/healthCheck-true", {
    test_that("healthCheck() returns correct message when up", {
        expect_message(healthCheck(), "The LFPS API is available")
    })
})

# httptest2::with_mock_dir("_mock/healthCheck-false", {
#     test_that("healthCheck() returns correct message when down", {
#         expect_warning(healthCheck(), "The LFPS API is currently down.
# Please notify the LANDFIRE helpdesk at <helpdesk@landfire.gov> of the following error:
# The service is down")
#     })
# })