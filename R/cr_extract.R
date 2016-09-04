#' Extract a variable from a Cost Report
#' 
#' This function takes a 5-column alpha-numeric dataset or numeric dataset from 
#' the Medicare cost reports, which are stored in a long format, and subsets 
#' them based on the worksheet number, line number, and column number provided. 
#' If desired, it will rename the resulting variable to whatever the user 
#' chooses.
#' 
#' It does not automatically adjust for the same variable having different rows 
#' / columns in Medicare data formatted for the 1996 vs 2010 form. The user may 
#' have to use this function twice, once on each source of data, to extract one 
#' variable over time.
#' 
#' It does automatically recode rows and columns into all possible permutations
#' (ie '500', '0500', '00500', 500) when subsetting, since different cost
#' reports use different schema.
#' 
#' @param dataset The name of a cost report alpha or numeric dataset
#' @param worksheet The name of the workheet, converted to 7-character format
#' @param row The row number of the data, as it appears in the Medicare workbook
#'   or documentation
#' @param column The column number of the data, as it appears in the Medicare 
#'   workbook
#' @param newname The name given to the variable that appears as a result of 
#'   this extraction
#' @return A 2-column dataset: one with the cost report \code{rpt_rec_number}, 
#'   used to merge data, and a column of the data requested, which is renamed if
#'   desired.
#'   
#' @examples 
#' 
#' alpha_data <- cr_hospice_2014_alpha
#' hospice_name <- cr_extract(alpha_data, "S100000", 100, 100, "name")

cr_extract <- function(dataset,
                       worksheet,
                       row,
                       column,
                       newname = 'newvar') {
  # Check that dataset has 5 columns, which all cost report alpha and nmrc
  # datasets do
  if (ncol(dataset) < 5) {
    stop("Dataset is not the expected size. Please only feed alpha or nmrc datasets")
  }
  
  # subset the data, being flexible with row / column since the cost reports are
  # inconsistent themselves
  no_zeroes_row <- gsub("^0*", "", row)
  one_zero_row <- paste0("0", no_zeroes_row)
  two_zero_row <- paste0("00", no_zeroes_row)
  # some rows in some sources have alphanumeric data. Try to convert them to
  # numeric, and if it fails, silently return NA
  
  numeric_row <- tryCatch({as.numeric(row)}, 
                      error = function(x) return(NA), 
                      warning = function(x) return(NA))
  
  no_zeroes_col <- gsub("^0*", "", column)
  one_zero_col <- paste0("0", no_zeroes_col)
  two_zero_col <- paste0("00", no_zeroes_col)
  # some rows in some sources have alphanumeric data. Try to convert them to
  # numeric, and if it fails, silently return NA
  numeric_col <- tryCatch({as.numeric(column)}, 
                          error = function(x) return(NA), 
                          warning = function(x) return(NA))
  
  data_subset <- dataset[dataset[, 2] == worksheet, ]
  if (nrow(data_subset) == 0) {
    warning("No data found with specified worksheet. Double-check and ensure it matches Medicare's 7-character style.")
  }
  
  data_subset_2 <- data_subset[data_subset[, 3] %in% list(no_zeroes_row, one_zero_row,
                                                       two_zero_row, numeric_row) &
                                 !is.na(data_subset[, 3]), ]
  if (nrow(data_subset_2) == 0) {
    warning("No data found with specified row number.")
  }
  
  data_subset_3 <- data_subset_2[data_subset_2[, 4] %in% list(no_zeroes_col, one_zero_col,
                                                       two_zero_col, numeric_col) &
                                 !is.na(data_subset_2[, 4]), ]
  if (nrow(data_subset_3) == 0) {
    warning("No data found with specified column number.")
  }
  
  # Keep 2 columns: first is record linkage (don't rename it), second is renamed
  # and the desired variable
  final <- data_subset_3[, c(1, 5)]
  names(final) <- c(names(final)[1], newname)
  

  if (nrow(final) == 0) {
    warning("Final result has no data. Double-check parameters or consider switching alpha/nmrc extraction dataset.")
  }
  
  return(final)
}