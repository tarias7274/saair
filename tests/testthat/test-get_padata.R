is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

test_that("Accept dataframe input", {

  skip_if(identical(Sys.getenv("SAAIR_KEY"), ""))

  testing_data <- data.frame(
    sensor_index = "151664",
    read_key = "95OQ853QATISF7V8"
    )
  # Run Function
  test_mem <- get_padata(
    testing_data, fields = c("date_created")
    ) |> suppressMessages()
  # Establish expected output
  expected_output <- data.frame(
    sensor_index = 151664,
    date_created = as.Date("2022-06-29"),
    name = "UTSA W Campus"
    )
  expect_equal(test_mem, expected_output)
})

test_that("Accept vector input", {

  skip_if(identical(Sys.getenv("SAAIR_KEY"), ""))

  testing_data <- 151664

  # Run Function
  test_mem <- get_padata(
    testing_data, fields = c("date_created")
  ) |> suppressMessages()
  # Establish expected output
  expected_output <- data.frame(
    sensor_index = 151664,
    date_created = as.Date("2022-06-29"),
    name = "UTSA W Campus"
  )
  expect_equal(test_mem, expected_output)
})

test_that("Fails gracefully (no read key)", {

  skip_if(identical(Sys.getenv("SAAIR_KEY"), ""))

  testing_data <- 151840

  # Run Function
  test_mem <- get_padata(
    testing_data, fields = c("date_created")
  ) |> suppressMessages()
  expect_null(test_mem)
})

test_that("Take multiple vector input", {

  skip_if(identical(Sys.getenv("SAAIR_KEY"), ""))

  testing_data <- c(150454, 150840, 151568)

  # Run Function
  test_mem <- get_padata(testing_data)
  # Establish expected output
  expected_output <- data.frame(
    sensor_index = c(150454, 150840, 151568),
    name = c("Esteban Lopez", "Museum reach boat ramp", "ShavanoPark")
  )
  expect_equal(test_mem, expected_output)
})
