is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

test_that("Output matches expectation", {

  skip_if(identical(Sys.getenv("SAAIR_KEY"), ""))

  testing_data <- data.frame(
    sensor_index = "151664",
    read_key = "95OQ853QATISF7V8"
    )
  # Run Function
  test_mem <- get_padata(
    testing_data, fields = c("hardware", "date_created")
    ) |> suppressMessages()
  # Establish expected output
  expected_output <- data.frame(
    sensor_index = 151664,
    date_created = as.Date("2022-06-29"),
    name = "UTSA W Campus",
    hardware = "2.0+OPENLOG+31037 MB+PMSX003-B+PMSX003-A"
    )
  expect_equal(test_mem, expected_output)
})
