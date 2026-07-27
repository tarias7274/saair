#' Calibrate PurpleAir temperatures
#'
#' @param data A dataframe of PurpleAir data, such as output from
#' `load_padata()`
#' @param models Optional list of calibration models. If none are supplied, the
#' package's internal calibrations are used
#' @param calibration A character that best describes the deployment conditions.
#' All sensors in `data` dataframe should have the same condition
#'   "sunlit": sensor is deployed outdoors and is largely sunlit during the day
#'   "shaded": sensor is deployed outdoors and is largely shaded during the day
#'   "indoor": sensor is deployed indoors
#'   "shell" (unimplemented): sensor is contained in a 3D-printed shell
#'   deployed at a Via bus stop
#' @param temperature_col A character specifying the name of the Fahrenheit data
#' column if it's not "T_air"
#' @param rh_col A character specifying the name of the 0-100% relative humidity
#' data if it's not "RH"
#' @param time_col A character specifying the name of the timestamp column if
#' it's not "Timestamp_Local". See also timestamp_is_char
#' @param output_c_col A character specifying the name of the output Celsius
#' column. Default: "calT_C"
#' @param output_f_col A character specifying the name of the output Fahrenheit
#' column. Default: "calT_F"
#' @param timezone The local time zone for the data
#' @param timestamp_is_char If `TRUE`, `time_col` is treated as a local
#'   clock time even if R has assigned the wrong timezone attribute. This
#'   matches current `load_padata()` behavior.
#' @param warn_range Get warnings if it looks like your temperature or RH units
#' are wrong
#'
#' @returns A dataframe with calT_C and calT_F (calibrated temperatures
#' in C and F), PA_time_local, and PA_time_UTC appended to original data
#' @export
#'
#' @examples
#' \dontrun{
#' start_date <- as_datetime("2025-04-20", tz = "America/Chicago")
#' end_date <- as_datetime("2025-04-21", tz = "America/Chicago")
#'
#' pa_data <- load_padata(
#'   data_dir = PAfiles,
#'   load_interval = interval(start = start_date, end = end_date),
#'   indexes = c(150840, 151664)
#'   )
#'
#' models <- readRDS("PA_temperature_calibration_models.rds")
#'
#' pa_calibrated <- calpat(
#'   data = pa_data,
#'   models = models,
#'   calibration = "shaded"
#'   )
#'
#' ggplot(pa_calibrated) +
#'   theme_bw() +
#'   geom_point(aes(x = PA_time_local, y = T_air), color = "purple") +
#'   geom_point(aes(x = PA_time_local, y = calT_F), color = "forestgreen") +
#'   geom_point(aes(x = PA_time_local, y = calT_C), color = "blue") +
#'   facet_wrap(~sensor_index) +
#'   xlim(start_date, end_date)
#' }
calpat <- function(
    data,
    models = models,
    calibration = c("sunlit", "shaded", "indoor", "shell"),
    temperature_col = "T_air",
    rh_col = "RH",
    time_col = "Timestamp_Local",
    output_c_col = "calT_C",
    output_f_col = "calT_F",
    timezone = "America/Chicago",
    timestamp_is_char = TRUE,
    warn_range = TRUE
    ) {
  # Finishes partial arg matches, errors if none found or ambiguous
  calibration <- match.arg(calibration)
  # Check for missing package installs
  # required_pkgs <- c("dplyr", "lubridate", "stats")
  # missing_pkgs <- required_pkgs[
  #   !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
  #   ]
  #
  # if (length(missing_pkgs) > 0) {
  #   stop(
  #     "Missing required package(s): ",
  #     paste(missing_pkgs, collapse = ", "),
  #     call. = FALSE
  #   )
  # }
  # Basic column checks
  needed_cols <- c(temperature_col, rh_col, time_col)
  if (!all(needed_cols %in% names(data))) {
    stop(
      "Input data must contain: ",
      paste(needed_cols, collapse = ", "),
      ". If using non-standard column names, provide them in the function call",
      call. = FALSE
    )
  }
  # Catch unimplemented shell calibration
  if (calibration == "shell") {
    stop(
      "Remind Dr. Brown that she was supposed to add that to models.",
      call. = FALSE
    )
  }
  # if (calibration %notin% names(models)) {
  #   stop(
  #     "Calibration model not found in `models`: ",
  #     calibration,
  #     "\nAvailable models are: ",
  #     paste(names(models), collapse = ", "),
  #     call. = FALSE
  #   )
  # }
  # Pull appropriate model for based on choice argument
  model <- models[[calibration]]
  # Calculate Celsius temperatures and dupe rh column for calibration use
  data <- data |>
    mutate(
      PA_T_C = (.data[[temperature_col]] - 32) * 5/9,
      PA_RH = .data[[rh_col]]
    )
  # The current version of load_padata calls Timestamp_Local with a string
  # local time stored as text. This is read in as UTC so we need to use
  # force_tz()
  if (timestamp_is_char) {
    # Timestamp_Local has the right clock time but wrong timezone label
    data <- data |>
      mutate(
        PA_time_local = as_datetime(.data[[time_col]], tzone = timezone) |>
          suppressWarnings(),
        # Convert that corrected local time to UTC for matching
        PA_time_UTC = lubridate::with_tz(.data$PA_time_local, tzone = "UTC")
        )
    } else { # if is timestamp
      data <- data |>
        mutate(
          PA_time_UTC = lubridate::with_tz(.data[[time_col]], tzone = "UTC"),
          PA_time_local = lubridate::with_tz(
            .data$PA_time_UTC, tzone = timezone
            )
        )
      }
  # Create new object with fit vars
  out <- data |>
    mutate(
      time_decimal = lubridate::hour(.data$PA_time_local) +
        lubridate::minute(.data$PA_time_local) / 60,
      time_sin = sin(2 * pi * .data$time_decimal / 24),
      time_cos = cos(2 * pi * .data$time_decimal / 24),
      time_sin2 = sin(4 * pi * .data$time_decimal / 24),
      time_cos2 = cos(4 * pi * .data$time_decimal / 24)
    )
  # Optional sanity checks
  if (warn_range) {
    temp_q <- stats::quantile(out$PA_T_C, probs = c(0.01, 0.99), na.rm = TRUE)
    if (temp_q[2] < 20) {
      warning(
        "Input temperatures look too low for Fahrenheit. ",
        "Did you pass Celsius values? The function expects Fahrenheit.",
        "If you're sure it's just cold, set warn_range = FALSE",
        "If there's interest in an option to pass C, email Dr. Brown",
        call. = FALSE
      )
    } # Temptest

    rh_q <- stats::quantile(out$PA_RH, probs = c(0.05, 0.99), na.rm = TRUE)
    if (rh_q[1] < 1 || rh_q[2] > 100) {
      warning(
        "RH values fall outside the expected 1 to 100% range.",
        "If there's interest in an option to pass 0 to 1 RH, email Dr. Brown",
        call. = FALSE
        )
      # 0 to 1 is possible, but highly unlikely and
      # more likely to be fractional representation of a percentage
    } # RH test
  } # warn range


  #   ## May need for shell, other models need only req'd inputs
  #     # Check that model predictors are present
  # needed_model_vars <- all.vars(delete.response(stats::terms(model)))
  # missing_model_vars <- setdiff(needed_model_vars, names(out))

  # Predict calibrated temperature
  pred_c <- stats::predict(model, newdata = out)
  # Assign predicted temperatures to output object
  out[[output_c_col]] <- as.numeric(pred_c)
  out[[output_f_col]] <- out[[output_c_col]] * 9 / 5 + 32
  # Remove calibration helper data and output result
  out |> select(-c(
    "PA_T_C", "PA_RH", "time_decimal",
    "time_sin", "time_cos", "time_sin2", "time_cos2"
    ))
}

# get_pa_temp_models <- function(models = NULL) {
#   if (!is.null(models)) {
#     return(models)
#   }
#
#   if (!exists("pa_temp_calibration_models", envir = parent.env(environment()))) {
#     stop(
#       "Internal calibration models were not found.\n",
#       "Try reinstalling the package or pass `models` explicitly.",
#       call. = FALSE
#     )
#   }
#   pa_temp_calibration_models
# }
