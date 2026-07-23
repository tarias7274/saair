test_that("load works", {
  skip_if(
    stringr::str_split_1(system.file(package = "saair"), "/")[3] %notin%
      c("metro", "tarias7274")
    )
  test_mem <- load_padata(
    data_dir = system.file(
      package = "saair",
      "..", "..", "Air-Quality-Scratch", "0_RAW", "PurpleAir"
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
