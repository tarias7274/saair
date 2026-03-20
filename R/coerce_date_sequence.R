#' Coerce start/end dates into half-month sequence
#'
#' The coerce_date_sequence function accepts start and end dates in the format
#' year-mo-dy, pushing the start date into the past to the nearest valid 1st or
#' 16th of the month (inclusive) and pushes the end date into the past to the
#' nearest valid 1st of 16th of the month (exclusive). It then breaks the period
#' into two-week chunks that can be looped through to download multiple two-week
#' datasets.
#'
#' @param start_date,end_date POSIXt, Date, or a character vector. Attached timezone
#' doesn't matter, as it is overwritten by the time_zone argument
#' @param time_zone a recognized character timezone, optional; defaults to the
#' local system timezone. The inputs and resulting output will have their
#' timezone forced to `time_zone`'s value
#' @param limit a numeric value, optional; determines the furthest in the past
#' dates will be permitted
#' @param limit_unit a character, optional; unit of the limit numeric i.e.
#' "days", "months". Defaults to "days"
#'
#' @returns A vector of Date values which includes the `start_date`, `end_date`,
#' and any 1st and/or 16th of the months in-between
#' @export
#'
#' @examples
#' start_date <- as.Date("2023-01-01")
#' end_date <- as.Date("2024-01-01")
#' download_dates <- coerce_date_sequence(
#'   start_date = start_date,
#'   end_date = end_date,
#'   time_zone = "America/Chicago"
#' )
#' download_dates
coerce_date_sequence <- function(
  start_date, end_date, time_zone = Sys.timezone(),
  limit = NULL, limit_unit = "days"
) {
  date_today <- today(tzone = time_zone)
  start_date <- start_date |> force_tz(time_zone)
  end_date <- end_date |> force_tz(time_zone)
  # Coerce start date forward in time -----------------------------------------
  start_date <- start_pull(start_date)
  # Coerce end date backwards in time -----------------------------------------
  end_date <- end_push(end_date)
  # Fix start_date older than limit -------------------------------------------
  if (!is.null(limit)) {
    if (is.na(as.period(limit))) {
      sprintf(
        "attempt to coerce limit to period produced NA\n",
        "try again with a period or period equivalent"
      ) |> cli_alert_danger()
      stop()
    }
    if (!is.period(limit)) {
      limit <- limit |>
        as.period(unit = limit_unit)
    }
    if (start_date < (date_today - limit) & end_date < (date_today - limit)) {
      sprintf(
        paste(
          "Entire date range is prior to download limit of %s",
          "%s: today - %s days",
          "Use more recent dates",
          sep = "\n"
        ),
        limit,
        start_date,
        difftime(today(), start_date, units = "days") |> floor()
      ) |> cli_alert_danger()
      stop()
    }
    if (start_date < (date_today - limit)) {
      sprintf(
        paste(
          "Date period starts prior to download limit of %s",
          "%s: today - %s days",
          "Finding most recent valid download start date",
          sep = "\n"
        ),
        limit,
        start_date,
        difftime(today(), start_date, units = "days") |> floor()
      ) |> cli_alert_info()
      start_date_reference <- start_date
      start_date <- date_today - limit
      start_date <- start_pull(start_date)
      sprintf(
        "Date moved: %s -> %s (Today - %s days)",
        start_date_reference,
        start_date,
        floor(difftime(today(), start_date, units = "days"))
      ) |> cli_alert_success()
    }
  }
  # Fix EndDate in the future --------------------------------------------------
  if (end_date > date_today) {
    cli_alert_info("End date in future. Setting to most recent valid date.")
    end_date_reference <- end_date
    end_date <- date_today
    end_date <- end_push(end_date)
    sprintf(
      "Date moved: %s -> %s",
      end_date_reference,
      end_date
    ) |> cli_alert_success()
  }
  # Check invalid dates --------------------------------------------------------
  if (start_date == end_date) {
    cli_alert_danger(
      "Start and end dates are the same, try sampling a wider period"
    )
  } else if (start_date > end_date) {
    cli_alert_info("Start date is after end date, swapping...")
    start_date_2 <- start_date
    start_date <- end_date
    end_date <- start_date_2
    rm(start_date_2)
  }
  # Break date block into smaller bits ----------------------------------------
  if(day(start_date) == 1) {
    download_dates <- c(
      start_date,
      seq.Date(start_date, end_date, by = "month"),
      seq.Date(start_date + days(15), end_date, by = "month"),
      end_date
    ) |> unique() |> sort()
  } else if (day(start_date) == 16) {
    download_dates <- c(
      start_date,
      seq.Date(start_date, end_date, by = "month"),
      seq.Date(start_date - days(15), end_date, by = "month"),
      end_date
    ) |> unique() |> sort()
  }
  download_dates <- download_dates[
    download_dates >= start_date & download_dates <= end_date
  ]
  return(download_dates)
}

start_pull <- function (x) {
  # Coerce start date forward in time -----------------------------------------
  if(day(x) != 1 && day(x) != 16) {
    x <- lubridate::ceiling_date(
      x, unit = ifelse(day(x) > 16, "1 month", "15 days")
    )
  }
  return(x)
}

end_push <- function (x) {
  # Coerce end date backwards in time -----------------------------------------
  if(day(x) != 1 && day(x) != 16) {
    # account for day 31 edge case that floors to 31
    if(day(x) == 31) x <- x - days(1)
    x <- lubridate::floor_date(
      x, unit = ifelse(day(x) > 16, "15 days", "1 month")
    )
  }
  return(x)
}
