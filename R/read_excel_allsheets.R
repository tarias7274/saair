#' Read all sheets in xlsx to list
#'
#' Functions similarly to [readxl()] in that it bundles the function to apply
#' on every sheet of an excel file, loading the results into a list with a
#' member per sheet
#'
#' @param filename A character filepath ending with the excel file to be loaded
#'
#' @returns A list of dataframes, one per sheet
#' @export
#'
#' @examples
#' \dontrun{
#' read_excel_allsheets("C:/MyExcel.xlsx")
#' }
read_excel_allsheets <- function (filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(x) readxl::read_excel(filename, sheet = x))
  x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
