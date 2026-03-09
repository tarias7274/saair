get_sensor_data <- function(sensors_df, fields, api_read_key) {
  # Run datatype checks -------------------------------------------------------
  break_bool <- FALSE
  if (!is.data.frame(sensors_df)) {
    cli_alert_danger("Incompatible type: sensors_df is not a dataframe")
    break_bool <- TRUE
  }
  if (!is.vector(fields)) {
    cli_alert_danger("Incompatible type: fields is not a vector")
    break_bool <- TRUE
  }
  if (!is.character(fields)) {
    cli_alert_danger("Incompatible type: fields is not a character")
    break_bool <- TRUE
  }
  if (!is.character(api_read_key)) {
    cli_alert_danger("Incompatible type: api_read_key is not a character")
    break_bool <- TRUE
  }
  if (break_bool == TRUE) break
  # Get point total before download
  org_start <- GET(
    "https://api.purpleair.com/v1/organization",
    add_headers("X-API-Key" = api_read_key)
  ) |>
    content(as = "parsed")
  # Start message
  sprintf(
    "Downloading Latest Data from %.0f PurpleAir Sensors",
    nrow(sensors_df)
  ) |> cli_alert_info()
  # Initialize progress bar with first sensor index
  sensor_id <- sensors_df$sensor_index[1]
  sensor_name <- sensors_df$Name[1]
  cli_progress_bar(
    total = nrow(sensors_df),
    format = paste0(
      "Sensor \"{sensor_name}\" SID{sensor_id} [{pb_current}/{pb_total}] ",
      "{pb_bar} {pb_percent} | ETA:{pb_eta}"
    )
  )
  # Create empty object to store returned data in
  pa_sensor_data <- c()
  # Begin loop through sensor dataframe data indexes
  for (i in seq_len(nrow(sensors_df))) {
    sensor_id <- sensors_df$sensor_index[i]
    sensor_mac <- sensors_df$MAC_SN[i]
    sensor_name <- sensors_df$Name[i]
    sensor_key <- sensors_df$read_key[i]
    # Check for missing sensor index for given mac
    if (is.na(sensor_id)) {
      cli_alert_info(
        sprintf(
          paste(
            "No data id for MAC: %s, \"%s\"",
            "If sensor is installed, manually add sensor index",
            sep = "\n"
          ),
          sensor_mac,
          sensor_name
        )
      )
      cli_progress_update()
      next
    }
    # Check for missing read_key
    if (!is.na(sensor_key)) {
      data_request <- GET(
        sub(
          ":sensor_index", sensor_id,
          "https://api.purpleair.com/v1/sensors/:sensor_index"
        ),
        query = list(
          read_key = sensor_key,
          fields = paste(fields, collapse = ",")
        ),
        add_headers("X-API-Key" = api_read_key)
      )
    } else {
      data_request <- GET(
        sub(
          ":sensor_index", sensor_id,
          "https://api.purpleair.com/v1/sensors/:sensor_index"
        ),
        query = list(
          fields = paste(fields, collapse = ",")
        ),
        add_headers("X-API-Key" = api_read_key)
      )
    }
    # Start time to avoid exceeding request rate limit
    benchmark_start <- Sys.time()
    # Parse request returned content
    data <- content(data_request, as = "parsed")
    if (data_request$status_code == 200) {
      # Data Parse Success Message!
      sprintf(
        "Data Request Success for SID%.0f: %s!",
        data$sensor$sensor_index,
        if_else(
          !is.null(data$sensor$name),
          data$sensor$name,
          NA_character_
        )
      ) |> cli_alert_success()
    } else if (data_request$status_code != 200) {
      # Data Parse Failure Message :(
      sprintf(
        paste(
          "Error in SID%.0f",
          "Error Code %.0f: %s\n%s",
          sep = "\n"
        ),
        sensor_id,
        data_request$status_code,
        data$error,
        data$description
      ) |> cli_alert_info()
      cli_progress_update()
      next
    }
    # Create dataframe from parse list
    data_df <- data.frame(data$sensor) |>
      mutate(
        MAC_SN = sensor_mac,
        across(
          any_of(contains(c("last", "date"))),
          ~ as_datetime(.x) |> date()
        )
      )
    # Create or append data to overall frame
    pa_sensor_data <- if (is.null(pa_sensor_data)) {
      data_df
    } else {
      full_join(pa_sensor_data, data_df)
    }
    # Suspend execution for appropriate time to avoid exceeding
    # API request's rate limit
    cli_progress_update()
    execution_time <- difftime(
      Sys.time(),
      benchmark_start,
      units = "secs"
    ) |>
      as.numeric()
    if (execution_time < 0.1) {
      Sys.sleep(time = 0.11 - execution_time)
    }
  }
  # Get point total after download
  org_end <- GET(
    "https://api.purpleair.com/v1/organization",
    add_headers("X-API-Key" = api_read_key)
  ) |>
    content(as = "parsed")
  # Output point usage report
  cat("\n\n\n")
  cli_alert_info(
    sprintf(
      paste(
        "POINT CONSUMPTION REPORT",
        "Download Account: %s",
        "Points Used: %.0f",
        sep = "\n"
      ),
      org_start$organization_name,
      org_start$remaining_points - org_end$remaining_points
    )
  )
  cat("\n\n\n")
  return(pa_sensor_data)
}
