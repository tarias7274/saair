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
#' \dontrun{
#' set_api_key()
#' }
set_api_key <- function(key = NULL) {
  if (is.null(key)) {
    key <- askpass::askpass("Please enter your API key")
  }
  Sys.setenv("SAAIR_KEY" = key)
}
