test_that("load works", {
  test_mem <- load_padata(
    data_dir = paste(
      "C:/Users/metro/OneDrive - University of Texas at San Antonio",
      "0_Documents/Air-Quality-Scratch/0_RAW/PurpleAir",
      sep = "/"
    ),
    load_interval = interval(
      as.Date("2026-06-01") |> force_tz("America/Chicago"),
      as.Date("2026-06-16") |> force_tz("America/Chicago")
    ),
    indexes = 151676
  )
  expected_output <- readRDS(test_path("fixtures", "load_padata_test_data.rds"))
  expect_equal(test_mem, expected_output, ignore_attr = TRUE)
})
