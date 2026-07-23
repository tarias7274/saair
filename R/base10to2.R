#' Base 10 to 2
#'
#' Converts any natural number (positive integer) to a vector of binary
#' bits
#'
#' @param base10_number A base 10 natural number
#'
#' @returns A vector of integers (1 or 0), with the rightmost element
#' representing the smallest bit
#' @export
#'
#' @examples
#' base10to2(50)
#' base10to2(98572)
base10to2 <- function(base10_number) {
  binary <- c()
  while (base10_number > 0) {
    binary <- c(binary, base10_number %% 2)
    base10_number <- floor(base10_number / 2)
  }
  rev(binary)
}
