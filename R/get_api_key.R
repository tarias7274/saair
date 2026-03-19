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
