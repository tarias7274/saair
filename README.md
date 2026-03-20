
<!-- README.md is generated from README.Rmd. Please edit that file -->

# saair

<!-- badges: start -->

[![R-CMD-check](https://github.com/tarias7274/saair/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tarias7274/saair/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of saair (San Antonio Air) is to assist scientists and
engineers in the Dr. Brown, Dr. Lopez-Ochoa, and related labs with
expediently and effectively studying heat and air quality in the City of
San Antonio, Texas. Formatting help code as a function enables more
robust and easy-to-maintain documentation and easy import into any code
file of any project without having to maintain disparate definitions
within several scripts or definitions across repositories or directories
with inconsistent access.

## Installation

You can install the development version of saair from
[GitHub](https://github.com/tarias7274/saair.git) with:

``` r
# install.packages("saair")
devtools::install_github("tarias7274/saair")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
if (FALSE) {
library(saair)
library(lubridate)
library(cli)
# Set path where function will find PurpleAir data
load_path <- Sys.getenv("EXAMPLE_PATH")
# Set start and end dates which will determine data pulled via overlap
start_date <- as_datetime("2023-01-01", tz = "America/Chicago")
end_date <- start_date + months(1)
# Check interval as a sanity check
sprintf(
    "Pulling PurpleAir Data Overlapping with Interval %s",
    interval(start = start_date, end = end_date)
  ) |>
  cli_alert_info()
# Call function with given parameters
pa_loaded_data <- load_padata(
  data_dir = load_path,
  interval(start = start_date, end = end_date),
  projects = c("EMME")
)
# Perform posthoc sanity checks
sprintf(
  "Pulled data ranges %s--%s, containing indexes %s",
  pa_loaded_data$Timestamp_Local |> min(),
  pa_loaded_data$Timestamp_Local |> max(),
  pa_loaded_data$sensor_index |> unique() |>
    paste(collapse = ", ")
) |> cli_alert_info()
# Write out resulting object to new file
if (FALSE) {
  write_csv(
    x = pa_loaded_data,
    file = paste(load_path, "PA-Subset_20230101_20230501.csv", sep = "/")
  )
}
}
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
# summary(cars)
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
