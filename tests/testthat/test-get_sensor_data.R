# load_test_data <- function() {
#   # Load decryption key environmental variable, if it exists
#   key <- Sys.getenv("SAAIR_KEY")
#   if (!identical(key, "")) {
#     return(key)
#   }
#   if (is_testing()) {
#     # Read in encrypted data fixture
#     testing_data <- here::here(
#       "tests", "testthat", "fixtures", "ListOfLocations_Clean.txt"
#       ) |>
#       secret_read_rds("SAAIR_KEY")
#     print(testing_data)
#       # as.data.frame() |>
#       # filter(sensor_index == 150454)
#     return(testing_data)
#   } else {
#     stop("No test data found, please supply")
#   }
# }

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

test_that("Output matches expectation", {
  testing_data <- data.frame(
    sensor_index = "151664",
    Name = "UTSA W Campus",
    read_key = "95OQ853QATISF7V8",
    MAC_SN = "30:83:98:B0:4D:F9"
  )
  test_mem <- testing_data |>
    get_sensor_data(
      fields = c("hardware", "date_created"),
      api_read_key = get_api_key()
    )
  expected_output <- data.frame(
    sensor_index = 151664,
    date_created = as.Date("2022-06-29"),
    hardware = "2.0+OPENLOG+31037 MB+PMSX003-B+PMSX003-A",
    MAC_SN = "30:83:98:B0:4D:F9"
  )
  expect_equal(test_mem, expected_output)
})
