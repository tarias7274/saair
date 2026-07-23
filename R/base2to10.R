#' Base 2 to 10
#'
#' Converts any vector of binary bits to its natural number equivalent
#'
#' @param base2_vector A vector of integers (1 or 0), with the rightmost element
#' representing the smallest bit
#'
#' @returns A base 10 natural number
#' @export
#'
#' @examples
#' base2to10(c(0,0,0,0,1,0))
#' base2to10(c(1,0,0,0,0,0))
#' base2to10(c(1,0,1,1,0,1))
base2to10 <- function(base2_vector) {
  num <- 0
  index <- length(base2_vector) - 1
  for (digit in base2_vector) {
    num <- num + (2 ^ index) * digit
    index <- index - 1
  }
  num
}
