#' Find if an element in Not In something
#'
#' Performs the reverse of the %in% operator, which is a binary operator of the
#' [match()] function, returning `TRUE` if a match is not found
#'
#'
#' @param x vector or NULL: the values to be matched. Long vectors are
#' supported.
#' @param table vector or NULL: the values to be matched against. Long vectors
#' are not supported.
#'
#' @returns A vector same length as x
#' @seealso [match()] which this function is the reverse of
#' @examples
#' 1:10 %notin% c(1, 3, 5, 9)
#' @export
"%notin%" <- function(x, table) {
  !("%in%"(x, table))
}
