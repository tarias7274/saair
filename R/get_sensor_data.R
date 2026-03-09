#' GET PurpleAir Sensor Data
#'
#' @param sensors_df A dataframe with PurpleAir sensor info
#' @param fields A character vector of api fields to return
#' @param api_read_key A valid character PurpleAir api read key
#' inheritParams
#'
#' @returns A dataframe with field values
#' @export
#'
#' @examples
#' sensors <- data.frame(
#'   sensor_index = 122948,
#'   MAC_SN = "00:00:00:00:00:00",
#'   Name = "PurpleSensor-0089",
#'   read_key = "28qhig4ghqh290qt"
#' )
#' fields_vector <- c("name", "uptime")
#' api_read <- "EHAIGEH1-18Y9-81H8-GGI9-HTQ238HQ9H8H"
#' get_sensor_data(sensors, fields_vector, api_read)
get_sensor_data <- function(sensors_df, fields, api_read_key) {
  # Run datatype checks -------------------------------------------------------
  break_bool <- FALSE
  if (!is.data.frame(sensors_df)) {
    cli::cli_alert_danger("Incompatible type: sensors_df is not a dataframe")
    break_bool <- TRUE
  }
  if (!is.vector(fields)) {
    cli::cli_alert_danger("Incompatible type: fields is not a vector")
    break_bool <- TRUE
  }
  if (!is.character(fields)) {
    cli::cli_alert_danger("Incompatible type: fields is not a character")
    break_bool <- TRUE
  }
  if (!is.character(api_read_key)) {
    cli::cli_alert_danger("Incompatible type: api_read_key is not a character")
    break_bool <- TRUE
  }
  if (break_bool == TRUE) stop()
  # Get point total before download
  org_start <- httr::GET(
    "https://api.purpleair.com/v1/organization",
    httr::add_headers("X-API-Key" = api_read_key)
  ) |>
    httr::content(as = "parsed")
  # Start message
  sprintf(
    "Downloading Latest Data from %.0f PurpleAir Sensors",
    nrow(sensors_df)
  ) |> cli::cli_alert_info()
  # Initialize progress bar with first sensor index
  sensor_id <- sensors_df$sensor_index[1]
  sensor_name <- sensors_df$Name[1]
  cli::cli_progress_bar(
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
      cli::cli_alert_info(
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
      cli::cli_progress_update()
      next
    }
    # Check for missing read_key
    if (!is.na(sensor_key)) {
      data_request <- httr::GET(
        sub(
          ":sensor_index", sensor_id,
          "https://api.purpleair.com/v1/sensors/:sensor_index"
        ),
        query = list(
          read_key = sensor_key,
          fields = paste(fields, collapse = ",")
        ),
        httr::add_headers("X-API-Key" = api_read_key)
      )
    } else {
      data_request <- httr::GET(
        sub(
          ":sensor_index", sensor_id,
          "https://api.purpleair.com/v1/sensors/:sensor_index"
        ),
        query = list(
          fields = paste(fields, collapse = ",")
        ),
        httr::add_headers("X-API-Key" = api_read_key)
      )
    }
    # Start time to avoid exceeding request rate limit
    benchmark_start <- Sys.time()
    # Parse request returned content
    data <- httr::content(data_request, as = "parsed")
    if (data_request$status_code == 200) {
      # Data Parse Success Message!
      sprintf(
        "Data Request Success for SID%.0f: %s!",
        data$sensor$sensor_index,
        dplyr::if_else(
          !is.null(data$sensor$name),
          data$sensor$name,
          NA_character_
        )
      ) |> cli::cli_alert_success()
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
      ) |> cli::cli_alert_info()
      cli::cli_progress_update()
      next
    }
    # Create dataframe from parse list
    data_df <- data.frame(data$sensor) |>
      dplyr::mutate(
        MAC_SN = sensor_mac,
        dplyr::across(
          c("last", "date") |>
            dplyr::contains() |>
            dplyr::any_of(),
          ~ as_datetime(.x) |> date()
        )
      )
    # Create or append data to overall frame
    pa_sensor_data <- if (is.null(pa_sensor_data)) {
      data_df
    } else {
      dplyr::full_join(pa_sensor_data, data_df)
    }
    # Suspend execution for appropriate time to avoid exceeding
    # API request's rate limit
    cli::cli_progress_update()
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
  org_end <- httr::GET(
    "https://api.purpleair.com/v1/organization",
    httr::add_headers("X-API-Key" = api_read_key)
  ) |>
    httr::content(as = "parsed")
  # Output point usage report
  cat("\n\n\n")
  cli::cli_alert_info(
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
