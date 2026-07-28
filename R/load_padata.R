#' Load saved PurpleAir data
#'
#' Loads purpleair(PA) sensor data based on the union of input sensor
#' indexes, names, and project. Stitches files into dataframe for analysis or
#' output.
#'
#' @param data_dir A character path from pc root to directory containing pa
#' data folders and `ListOfLocations_Clean.csv` sheet. The values in the
#' `sensor_index`, `Name`, and `Project` columns are referenced in this
#' function's matching arguments
#' @param load_interval A lubridate interval that loads any data files which
#' have intervals which overlap with it
#' @param indexes A vector of character or numeric purpleair data indexes. Valid
#' indexes are always six digits long, but may include a seventh some time in
#' the future
#' @param names A vector of character purpleair sensor names. This argument and
#' `projects` accept partial, case-insensitive matching
#' @param projects A vector of character pa projects. This argument and
#' `names` accept partial, case-insensitive matching
#' @param all_sensors A boolean which confirms no filtering. If data from all
#' sensors is desired, input no values for indexes/names/projects and set
#' this to `TRUE`
#' @param verbose A boolean which disables(default) or enables cli progress
#' bar updating on loading progress
#' @param level A character which specifies the level of data to be loaded
#'   "0": Raw data
#'   "1": Quality-assured data
#'   "2": Analysis-ready data
#' @param letter_mod An optional character that will be appended to the level
#' to specify an alternate directory. e.g. "B" -> "2B_ANALYSIS"
#' @param data_suffix An optional character that will be used instead of the
#' default for a certain level file suffix. Defaults for 0, 1, and 2 levels are
#' "RAW", "VAL", and "1HR", respectively
#'
#' @returns A dataframe containing all pa data matching selection criteria
#' @export
#'
#' @examples
#' \dontrun{
#' load_padata()
#' }
load_padata <- function(
  data_dir, load_interval, indexes = NULL, names = NULL, projects = NULL,
  all_sensors = FALSE, verbose = FALSE, level = "2",
  letter_mod = "", data_suffix = NULL
) {
  bug_bool <- FALSE
  # Make sure parameter classes are correct
  if (!is.character(data_dir)) {
    sprintf(
      "supplied data_dir parameter was of %s class\n",
      "data_dir must be of the character class",
      class(data_dir)
    ) |> cli_alert_danger()
    bug_bool <- TRUE
  }
  if (length(data_dir) > 1) {
    sprintf(
      "supplied %.0f data_dir strings\n",
      "only supply one directory string",
      length(data_dir)
    ) |> cli_alert_danger()
    bug_bool <- TRUE
  }
  if (!is.interval(load_interval)) {
    sprintf(
      "supplied load_interval parameter was of %s class\n",
      "load_interval must be of lubridate interval class",
      class(load_interval)
    ) |> cli_alert_danger()
    bug_bool <- TRUE
  }
  # Check selection vars if any are supplied
  if (any(!is.null(indexes), !is.null(names), !is.null(projects))) {
    if (!is.null(indexes)) {
      if (!is.vector(indexes)) {
        cli_alert_danger("indexes must be a vector")
        bug_bool <- TRUE
      }
      if (any(nchar(indexes) != 6)) {
        cli_alert_danger("indexes must be exactly 6 digits long")
        bug_bool <- TRUE
      }
    }
    if (!is.null(names)) {
      if (!is.vector(names)) {
        cli_alert_danger("names must be a vector")
        bug_bool <- TRUE
      }
      if (!is.character(names)) {
        sprintf(
          "supplied names parameter were of %s class\n",
          "names must be of the character class",
          class(names)
        ) |> cli_alert_danger()
        bug_bool <- TRUE
      }
    }
    if (!is.null(projects)) {
      if (!is.vector(projects)) {
        cli_alert_danger("projects must be a vector")
        bug_bool <- TRUE
      }
      if (!is.character(projects)) {
        sprintf(
          "supplied projects parameter were of %s class\n",
          "projects must be of the character class",
          class(projects)
        ) |> cli_alert_danger()
        bug_bool <- TRUE
      }
    }
  } else if (all_sensors == FALSE) {
    sprintf(
      "no sensor selection parameters supplied\n",
      "set all_sensors parameter to TRUE to load data from all sensors"
    ) |> cli_alert_warning()
    bug_bool <- TRUE
  }
  # Break function if any bugs detected
  if (bug_bool == TRUE) stop()
  # Get locations_info for getting indexes from names or projects
  locations_info <- data_dir |>
    paste("ListOfLocations_Clean.csv", sep = "/") |>
    read_csv(
      col_types = readr::cols(
        .default = "?", sensor_index = "i", Name = "c", read_key = "c",
        Lat = "d", Long = "d", Network = "f", MAC = "c", Model = "f",
        Storage_GB = "i", Privacy = "f", wifi_access = "l",
        install_date = "D", uninstall_date = "D", placement = "f",
        PA_order = "f", Project = "c", Address = "c",
        ContactName = "c", ContactPhone = "c", ContactEmail = "c"
      )
    )
  # Override/Skip specific selections if all_sensors set to TRUE
  if (all_sensors == TRUE) {
    indexes <- locations_info |>
      filter(!is.na(.data$sensor_index)) |>
      pull("sensor_index")
  } else {
    # Convert names to indexes
    if (!is.null(names)) {
      name_indexes <- locations_info |>
        filter(
          grepl(
            pattern = toupper(paste(names, collapse = "|")),
            toupper(.data$Name)
          ),
          !is.na(.data$sensor_index)
        ) |>
        pull("sensor_index")
      if (!is.null(indexes)) {
        indexes <- c(indexes, name_indexes) |>
          unique() |>
          sort()
      } else {
        indexes <- name_indexes |>
          unique() |>
          sort()
      }
    }
    # Convert project to indexes
    if (!is.null(projects)) {
      project_indexes <- locations_info |>
        filter(
          grepl(
            pattern = toupper(paste(projects, collapse = "|")),
            toupper(.data$Project)
          ),
          !is.na(.data$sensor_index)
        ) |>
        pull("sensor_index")
      if (!is.null(indexes)) {
        indexes <- c(indexes, project_indexes) |>
          unique() |>
          sort()
      } else {
        indexes <- project_indexes |>
          unique() |>
          sort()
      }
    }
  }
  if (length(indexes) == 0) {
    # Break function if no sensors detected
    cli_alert_danger("No sensors found for selection criteria")
    stop()
  }
  # Filtering for time and sensor ---------------------------------------------
  data_level <- dplyr::case_when(
    level == 0 ~ "0%s_RAW",
    level == 1 ~ "1%s_VALIDATED",
    level == 2 ~ "2%s_ANALYSIS"
    ) |> sprintf(letter_mod)
  folder_suffix <- dplyr::case_when(
    level == 0 ~ "RAW",
    level == 1 ~ "VAL",
    level == 2 ~ "ANA"
  )
  if (is.null(data_suffix)) {
    data_suffix <- dplyr::case_when(
      level == 0 ~ "RAW",
      level == 1 ~ "VAL",
      level == 2 ~ "1HR"
    )
  }
  # Reference against desired folder sequence
  folder_sequence <- coerce_date_sequence(
    lubridate::int_start(load_interval), lubridate::int_end(load_interval),
    ignore.paradox = TRUE, reverse.force = TRUE
  )
  folder_sequence <- data.frame(
    folder_name = paste(
      "PA",
      utils::head(folder_sequence, -1) |>
        stringr::str_remove_all("-") |>
        str_sub(end = 8),
      utils::tail(folder_sequence, -1) |>
        stringr::str_remove_all("-") |>
        str_sub(end = 8),
      folder_suffix,
      sep = "_"
    ),
    folder_interval = interval(
      start = utils::head(folder_sequence, -1),
      end = utils::tail(folder_sequence, -1)
    )
  )
  # Get folders that are in chosen data directory
  data_dir_test <- paste(data_dir, data_level, "PurpleAir", sep = "/")
  data_dir <- if_else(
    file.exists(data_dir_test),
    data_dir_test,
    paste(data_dir, data_level, sep = "/")
    )
  rm(data_dir_test)
  existing_folders <- data_dir |>
    list.files(pattern = paste0(folder_suffix, "$"))
  folder_matches <- folder_sequence |>
    filter(.data$folder_name %in% existing_folders) |>
    pull("folder_name")
  # Output feedback if function is verbose
  if (verbose) {
    sprintf(
      "Loading Data from %.0f PurpleAir Sensors for %s",
      length(indexes),
      load_interval
    ) |> cli_alert_info()
    # Create progress bar if function is verbose
    options(cli.progress_show_after = 0)
    cli_progress_bar(
      total = length(folder_matches) * length(indexes),
      format = paste0(
        "Sensor [{pb_current}/{pb_total}] ",
        "{pb_bar} {pb_percent} | ETA:{pb_eta}"
      )
    )
  }
  # Actually pull the data ----------------------------------------------------
  # Create empty data to append to
  data_out <- data.frame(
    sensor_index = integer(),
    Timestamp_Local = character(),
    RSSI = integer(),
    RH = integer(),
    T_air = integer(),
    Abs_P = double(),
    PM2.5_A = double(),
    PM2.5_B = double()
  )
  for (folder_match in folder_matches) {
    # Get file names that
    data_files <- paste(data_dir, folder_match, sep = "/") |>
      list.files(pattern = paste0(data_suffix, "[.]csv$"))
    data_files <- data_files |>
      grep(pattern = paste(indexes, collapse = "|"), value = TRUE)
    found_sensors <- data_files |>
      str_sub(start = 7, end = 12) |>
      as.numeric()
    missing_sensors <- indexes[indexes %notin% found_sensors]
    # Output missing sensor feedback if function is verbose
    if ((length(missing_sensors) > 0) & verbose) {
      sprintf(
        "No data found for sensor(s) %s in %s. Skipping",
        paste(missing_sensors, collapse = ", "),
        folder_match
      ) |> cli_alert_info()
      # Update progress bar if function is verbose
      for (i in length(missing_sensors)) {
        cli_progress_update()
      }
    }
    for (data_file in data_files) {
      data_append <- paste(data_dir, folder_match, data_file, sep = "/") |>
        read_csv(
          col_types = readr::cols(
            .default = "?", sensor_index = "i", Timestamp_Local = "c",
            RSSI = "i", RH = "i", T_air = "i",
            Abs_P = "d", PM2.5_A = "d", PM2.5_B = "d"
          )
        )
      # Check for saved datetime format
      data_append <- data_append |>
        mutate(
          # Correct timestamps missing colon in UTC offset
          Timestamp_Local = if_else(
            str_sub(.data$Timestamp_Local, end = -5) %in% c("-0500", "-0600"),
            str_sub(.data$Timestamp_Local, end = -3) |> paste0(":00"),
            .data$Timestamp_Local
          ),
          # Correct timestamps that were saved in posix
          Timestamp_Local = if_else(
            str_sub(
              .data$Timestamp_Local, end = -5
              ) %notin% c("-05:00", "-06:00"),
            as_datetime(.data$Timestamp_Local, tz = "America/Chicago") |>
              lubridate::format_ISO8601(usetz = TRUE) |>
              str_sub(end = -3) |>
              paste0(":00"),
            .data$Timestamp_Local
          )
          # Ignore duplicate rows from incorrect past treatment of datetimes
        ) |>
        suppressMessages() |>
        dplyr::distinct()
      data_out <- data_out |>
        full_join(
          data_append,
          by = intersect(colnames(data_out), colnames(data_append))
          )
      rm(data_append)
      # Update progress bar if function is verbose
      if (verbose) cli_progress_update()
    }
  }
  data_out
}
