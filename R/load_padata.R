#' Load saved PurpleAir data
#'
#' Loads raw purpleair(PA) sensor data based on the union of input sensor
#' indexes, names, and project. Stitches files into dataframe for analysis or
#' output.
#'
#' @param data_dir A character path from pc root to directory containing pa data
#' folders
#' @param load_interval A lubridate interval that loads any data files which
#' have intervals which overlap with it
#' @param indexes A vector of character or numeric pa data indexes
#' @param pa_names A vector of character pa names
#' @param projects A vector of character pa projects
#' @param all_sensors A boolean which confirms no filtering. If data from all
#' sensors is desired, input no values for indexes/pa_names/projects and set
#' this to `TRUE`
#'
#' @returns A dataframe containing all pa data matching selection criteria
#' @export
#'
#' @examples
#' \dontrun{
#' load_padata()
#' }
load_padata <- function(
  data_dir, load_interval, indexes = NULL, pa_names = NULL, projects = NULL,
  all_sensors = FALSE
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
  if (!(is.null(indexes) && is.null(pa_names) && is.null(projects))) {
    if (!is.null(indexes)) {
      if (!is.vector(indexes)) {
        cli_alert_danger("indexes must be a vector")
        bug_bool <- TRUE
      }
    }
    if (!is.null(pa_names)) {
      if (!is.vector(pa_names)) {
        cli_alert_danger("names must be a vector")
        bug_bool <- TRUE
      }
      if (!is.character(pa_names)) {
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
    read_csv()
  # Override/Skip specific selections if all_sensors set to TRUE
  if (all_sensors == TRUE) {
    indexes <- locations_info |>
      filter(!is.na(.data$sensor_index)) |>
      pull("sensor_index")
  } else {
    # Convert names to indexes
    if (!is.null(pa_names)) {
      name_indexes <- locations_info |>
        filter(
          .data$Name %in% pa_names,
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
  # Filtering for time and sensor ---------------------------------------------
  # Get folders that are in chosen data directory
  existing_folders <- list.files(data_dir) |>
    grep(pattern = ".csv|.xlsx", value = TRUE, invert = TRUE) |>
    grep(pattern = "RAW", value = TRUE) |>
    data.frame() |>
    setNames("folder")
  # Get subset of folders that contain data overlapping with
  # defined time period
  folder_matches <- existing_folders |>
    mutate(
      interval = str_sub(.data$folder, start = 4, end = 20),
      start = str_sub(.data$interval, end = 8) |>
        as_datetime() |>
        force_tz(tzone = "America/Chicago"),
      end = str_sub(.data$interval, start = -8) |>
        as_datetime() |>
        force_tz(tzone = "America/Chicago"),
      interval = interval(
        .data$start + days(1),
        .data$end - days(1)
      )
    ) |>
    select("folder", "interval") |>
    filter(
      int_overlaps(.data$interval, load_interval)
    ) |>
    pull("folder")
  sprintf(
    "Loading Data from %.0f PurpleAir Sensors for %s",
    length(indexes),
    load_interval
  ) |> cli_alert_info()
  options(cli.progress_show_after = 0)
  cli_progress_bar(
    total = length(folder_matches) * length(indexes),
    format = paste0(
      "Sensor [{pb_current}/{pb_total}] ",
      "{pb_bar} {pb_percent} | ETA:{pb_eta}"
    )
  )
  # Actually pull the data ----------------------------------------------------
  data_out <- c()
  for (folder_match in folder_matches) {
    # Get file names that
    data_files <- paste(data_dir, folder_match, sep = "/") |>
      list.files() |>
      grep(pattern = ".csv", value = TRUE) |>
      grep(pattern = paste(indexes, collapse = "|"), value = TRUE)
    found_sensors <- data_files |>
      str_sub(start = 7, end = 12) |>
      as.numeric()
    missing_sensors <- indexes[indexes %notin% found_sensors]
    if (length(missing_sensors) > 0) {
      sprintf(
        "No data found for sensor(s) %s in %s. Skipping",
        paste(missing_sensors, collapse = ", "),
        folder_match
      ) |> cli_alert_info()
      for (i in length(missing_sensors)) {
        cli_progress_update()
      }
    }
    for (data_file in data_files) {
      data_append <- paste(data_dir, folder_match, data_file, sep = "/") |>
        read_csv()
      if (is.null(data_out)) {
        data_out <- data_append
      } else {
        data_out <- full_join(
          data_out,
          data_append,
          by = colnames(data_out)
        )
      }
      cli_progress_update()
    }
  }
  return(data_out)
}
