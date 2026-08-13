#' GET latest PurpleAir sensor data
#'
#' `get_padata()` allows a user to easily GET the latest field data for a
#' group of sensors by supplying sensor identifying information. Uses the
#' PurpleAir Get Sensor Data api request to pull data from PurpleAir
#'
#' @param sensors Either a `vector` or `dataframe` specifying sensors to GET
#'   * vector: a `vector` of `numeric` sensor indexes
#'   * dataframe: if any sensors are private, read keys will be needed to GET
#'   their data. In this case, a dataframe with `numeric` sensor_index and
#'   `character` read_key columns will be needed
#' @param fields A `character vector` of api fields to return; valid fields can
#' be found at https://api.purpleair.com/#api-sensors-get-sensor-data and
#' https://community.purpleair.com/t/api-fields-descriptions/4652. If no fields
#' are supplied, function will just return names associated with sensor indexes
#' @param api_read_key A valid `character` PurpleAir api read key. The function
#' will grab an existing read key if saved to your R environment with
#' `set_api_key()`
#' @param verbose Default: `FALSE`. A `boolean` that allows feedback messages
#' regarding pull successes & progress to be displayed
#' @param verbose_bug Default: `FALSE`. A `boolean` that allows messages
#' relevant to bugfixing the function to be displayed
#'
#' @returns A dataframe with field values for each supplied sensor
#' @export
#'
#' @examples
#' # Dummy example inputs which will fail if actually run
#' \dontrun{
#' sensors <- data.frame(
#'   sensor_index = 122948,
#'   read_key = "28qhig4ghqh290qt"
#' )
#' fields_vector <- c("name", "uptime")
#' api_read_key <- "EHAIGEH1-18Y9-81H8-GGI9-HTQ238HQ9H8H"
#' get_padata(sensors, fields_vector, api_read_key)
#' }
get_padata <- function(
    sensors, fields = "name", api_read_key = get_api_key(),
    verbose = FALSE, verbose_bug = FALSE
    ) {
  # Run datatype checks -------------------------------------------------------
  if (verbose_bug) cli_alert_info(paste("Initialize time", Sys.time()))
  bug_bool <- FALSE
  if (!is.data.frame(sensors) & !is.vector(sensors)) {
    cli::cli_alert_danger(
      "Incompatible type: sensors is neither a vector nor a dataframe"
      )
    bug_bool <- TRUE
  }
  if (!is.vector(fields)) {
    cli::cli_alert_danger("Incompatible type: fields is not a vector")
    bug_bool <- TRUE
  }
  if (!is.character(fields)) {
    cli::cli_alert_danger("Incompatible type: fields is not a character")
    bug_bool <- TRUE
  }
  if (!is.character(api_read_key)) {
    cli::cli_alert_danger("Incompatible type: api_read_key is not a character")
    bug_bool <- TRUE
  }
  # Convert sensor vector to dataframe for easier handling
  if (is.vector(sensors)) {
    sensors <- data.frame(sensor_index = sensors, read_key = NA)
  }
  # Check for numeric sensor indexes
  if (any(is.na(as.numeric(sensors$sensor_index)))) {
      cli_alert_danger(
        "Incompatible value: sensor indexes cannot be coerced to numeric"
      )
      bug_bool <- TRUE
  }
  # Check if any bug flags were raised & stop() if so
  if (bug_bool == TRUE) stop()
  if (verbose_bug) cli_alert_info(paste("End of input check", Sys.time()))
  # Convert sensor index column to character for string insertion functions
  sensors <- mutate(sensors, sensor_index = as.character(.data$sensor_index))
  TRACK_PROG <- nrow(sensors) > 50
  if (TRACK_PROG & verbose) {
    # Start message
    sprintf(
      "Downloading Latest Data from %.0f PurpleAir Sensors",
      nrow(sensors)
    ) |> cli_alert_info()
    # Initialize progress bar with first sensor index
    sensor_id <- sensors$sensor_index[1]
    cli_progress_bar(
      total = nrow(sensors),
      format = paste0(
        "Sensor SID{sensor_id} [{pb_current}/{pb_total}] ",
        "{pb_bar} {pb_percent} | ETA:{pb_eta}"
      )
    )
  }
  if (verbose_bug) cli_alert_info(
    paste("End org pull & progress bar", Sys.time())
    )
  # Create empty object to store returned data in
  pa_sensor_data <- c()
  # Add name for feedback purposes
  if ("name" %notin% fields) fields <- c("name", fields)
  # Begin loop through sensor dataframe data indexes
  for (row_num in seq_len(nrow(sensors))) {
    sensor_id <- sensors$sensor_index[row_num]
    sensor_key <- sensors$read_key[row_num]
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
        query = list(fields = paste(fields, collapse = ",")),
        httr::add_headers("X-API-Key" = api_read_key)
      )
    }
    # Start time to avoid exceeding request rate limit
    benchmark_start <- Sys.time()
    # Parse request returned content
    data <- httr::content(data_request, as = "parsed")
    if (data_request$status_code == 200 & verbose) {
      # Data Parse Success Message!
      sprintf(
        "Data Request Success for SID%.0f: %s!",
        data$sensor$sensor_index,
        if(!is.null(data$sensor$name)) {
          data$sensor$name
        } else {
          "No Name"
        }
      ) |> cli_alert_success()
    } else if (data_request$status_code != 200) {
      if (verbose) {
        # Data Parse Failure Message :(
        sprintf(
          paste(
            "Error in SID%s",
            "Error Code %.0f: %s\n%s",
            sep = "\n"
          ),
          sensor_id,
          data_request$status_code,
          data$error,
          data$description
        ) |> cli_alert_info()
        if (TRACK_PROG) cli_progress_update()
      }
      next
    }
    # Create dataframe from parse list
    data_df <- data.frame(data$sensor) |>
      mutate(
        dplyr::across(
          tidyselect::matches("last|date"),
          ~ as_datetime(.x) |> lubridate::date()
        )
      )
    # Create or append data to overall frame
    pa_sensor_data <- if (is.null(pa_sensor_data)) {
      data_df
    } else {
      full_join(pa_sensor_data, data_df, by = names(pa_sensor_data))
    }
    # Suspend execution for appropriate time to avoid exceeding
    # API request's rate limit
    if (TRACK_PROG & verbose) cli_progress_update()
    execution_time <- difftime(
      Sys.time(),
      benchmark_start,
      units = "secs"
    ) |>
      as.numeric()
    if (verbose_bug) cli_alert_info(paste("Execution Time:", execution_time))
    if (execution_time < 0.1) {
      Sys.sleep(time = 0.11 - execution_time)
    }
  }
  if (verbose_bug) cli_alert_info(paste("Exited loop", Sys.time()))
  # Get point total after download
  return(pa_sensor_data)
}
