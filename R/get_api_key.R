#' Get api key
#'
#' @returns Value for SAAIR_KEY environment variable if it exists in the user's environment
#' @export
#'
#' @examples
#' get_api_key()
get_api_key <- function() {
  key <- Sys.getenv("SAAIR_KEY")
  if (!identical(key, "")) {
    return(key)
  }

  if (is_testing()) {
    return(testing_key())
  } else {
    paste0(
      "No API key found, please supply with `api_key` argument or ",
      "run `set_api_key()` to set with SAAIR_KEY env var"
    ) |> stop()
  }
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

testing_key <- function() {
  httr2::secret_decrypt("0Nfl6u2KHYvlt7TEFmugKxUhwNVdGNA_Lhi2KPXM17hpJK383KkI-4S8zCMzIDDUusv80Q", "SAAIR_KEY")
}

#' Set api key
#'
#' Prompts user to set a PurpleAir API key. Typically follows this example format:
#' B144C732-11DE-11EE-A445-42098A784009
#'
#' @param key Kept NULL in order to use the obfuscating askpass interface
#'
#' @returns An action setting the env var `SAAIR_KEY` to input key value
#' @export
#'
#' @examples
#' set_api_key()
set_api_key <- function(key = NULL) {
  if (is.null(key)) {
    key <- askpass::askpass("Please enter your API key")
  }
  Sys.setenv("SAAIR_KEY" = key)
}
