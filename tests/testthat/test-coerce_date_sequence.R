test_that("Start date always has correct day", {
  test_dates <- seq.Date(
    from = as.Date("2023-12-01"),
    to = as.Date("2024-02-01"),
    by = "1 day"
  )
  expected_output <- c(
    as.Date("2023-12-01"),
    rep(as.Date("2023-12-16"), 15),
    rep(as.Date("2024-01-01"), 16),
    rep(as.Date("2024-01-16"), 15),
    rep(as.Date("2024-02-01"), 16)
  )
  # do test
  expect_equal(sapply(test_dates, start_pull), expected_output)
})

test_that("End date always has correct day", {
  test_dates <- seq.Date(
    from = as.Date("2023-12-01"),
    to = as.Date("2024-02-01"),
    by = "1 day"
  )
  expected_output <- c(
    rep(as.Date("2023-12-01"), 15),
    rep(as.Date("2023-12-16"), 16),
    rep(as.Date("2024-01-01"), 15),
    rep(as.Date("2024-01-16"), 16),
    rep(as.Date("2024-02-01"), 1)
  )
  # do test
  expect_equal(sapply(test_dates, end_push), expected_output)
})

test_that("Function properly handles flipped dates", {
  # start date (inclusive)
  start_date <- as_datetime("2024-01-01", tz = "America/Chicago")
  # end date (exclusive)
  end_date <- as_datetime("2023-01-01", tz = "America/Chicago")
  # run function & save
  download_dates <- coerce_date_sequence(
    start_date = start_date,
    end_date = end_date,
    time_zone = "America/Chicago"
  )
  # do test
  expected_output <- c(
    "2023-01-01", "2023-01-16", "2023-02-01", "2023-02-16", "2023-03-01",
    "2023-03-16", "2023-04-01", "2023-04-16", "2023-05-01", "2023-05-16",
    "2023-06-01", "2023-06-16", "2023-07-01", "2023-07-16", "2023-08-01",
    "2023-08-16", "2023-09-01", "2023-09-16", "2023-10-01", "2023-10-16",
    "2023-11-01", "2023-11-16", "2023-12-01", "2023-12-16", "2024-01-01"
  ) |> as_datetime(tz = "America/Chicago")
  # do test
  expect_equal(download_dates, expected_output)
})

test_that("Function properly handles date limits", {
  # end date (exclusive)
  end_date <- today(tzone = "America/Chicago")
  # start date (inclusive)
  start_date <- end_date - days(200)
  # run function & save
  download_dates <- coerce_date_sequence(
    start_date = start_date,
    end_date = end_date,
    time_zone = "America/Chicago",
    limit = 90
  )
  # do test
  expect_true(download_dates[1] > end_date - days(90))
})

test_that("Output matches expectation", {
  # start date (inclusive)
  start_date <- as_datetime("2023-01-01", tz = "America/Chicago")
  # end date (exclusive)
  end_date <- as_datetime("2024-01-01", tz = "America/Chicago")
  # run function & save
  download_dates <- coerce_date_sequence(
    start_date = start_date,
    end_date = end_date,
    time_zone = "America/Chicago"
  )
  expected_output <- c(
    "2023-01-01", "2023-01-16", "2023-02-01", "2023-02-16", "2023-03-01",
    "2023-03-16", "2023-04-01", "2023-04-16", "2023-05-01", "2023-05-16",
    "2023-06-01", "2023-06-16", "2023-07-01", "2023-07-16", "2023-08-01",
    "2023-08-16", "2023-09-01", "2023-09-16", "2023-10-01", "2023-10-16",
    "2023-11-01", "2023-11-16", "2023-12-01", "2023-12-16", "2024-01-01"
  ) |> as_datetime(tz = "America/Chicago")
  # do test
  expect_equal(download_dates, expected_output)
})
