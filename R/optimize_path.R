#' Optimize file path
#'
#' `optimize_path()` detects and removes elements from a file path using ".."
#' characters to perform up-directory moves, assuming a "/x/y/z" file path type.
#' Can be useful for shortening file paths created by `here()` function.
#'
#' @param path a character file path constructed of folder names separated by
#' "/" separators
#' @param feedback An optional boolean which suppresses/allows feedback on
#' removed path elements
#'
#' @returns A character file path that describes shortest possible path to a
#' directory
#' @export
#'
#' @examples
#' path <- paste(
#'   "C:", "Users", "CoolUser",
#'   "ReallyLongFolderWeDontUse", "ReallyLongFolderWeDontUse2ElectricBoogaloo",
#'   "..", "..", "FolderA",
#'   sep = "/"
#'   )
#' short_path <- optimize_path(path)
#' short_path
optimize_path <- function(path, feedback = FALSE) {
  path <- path |>
    stringr::str_split(pattern = "/") |>
    unlist()
  for (i in seq_along(path)) {
    if (is.na(path[i])) next
    if (grepl("\\.\\.", path[i])) {
      if (feedback == TRUE) {
        cli_alert_info(sprintf("Set \"%s\" to NA", path[i]))
      }
      path[i] <- NA
      if (feedback == TRUE) {
        sprintf(
          "Set \"%s\" to NA",
          path[utils::tail(which(!is.na(path[1:i])), 1)]
        ) |> cli_alert_info()
      }
      path[utils::tail(which(!is.na(path[1:i])), 1)] <- NA
    }
  }
  path <- path[!is.na(path)] |>
    paste(collapse = "/")
  return(path)
}
