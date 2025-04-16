# Test for checkStatus.R

test_that("`healthCheck()` returns helpful messages", {
    mock_response_pass = list(
        message = "The service is up",
        success = TRUE
    )
    mock_response_fail = list(
        message = "The service is down",
        success = FALSE
    )
})

httptest2::with_mock_api({
    testthat::expect_message(healthCheck(), "The LFPS API is available")
    testthat::expect_warning(healthCheck(), "The LFPS API is currently down.")
})