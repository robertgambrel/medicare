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
#' This function *does not* throw an error if the parameters yield an empty 
#' dataset at any point. It only gives warnings. This is because oftentimes the 
#' parameters are valid but the data is missing in the source material, due to 
#' CMS scrubbing of what data gets published.
#' 
#' @param dataset The name of a cost report alpha or numeric dataset
#' @param worksheet The name of the workheet, converted to 7-character format
#' @param row The row number of the data, as it appears in the Medicare workbook
#'   or documentation (i.e. at least 3 digits. Row 5 must be entered as 500, row
#'   5.1 as 501, etc.)
#' @param column The column number of the data, as it appears in the Medicare 
#'   workbook (same general rule as for rows)
#' @param newname The name given to the variable that appears as a result of 
#'   this extraction
#' @return A 2-column dataset: one with the cost report \code{rpt_rec_number}, 
#'   used to merge data, and a column of the data requested, which is renamed if
#'   desired.
#'   
#' @examples 
#' 
#' alpha_data <- hospiceALPHA
#' hospice_name <- cr_extract(alpha_data, "S100000", 100, 100, "name")
#' 
#' @export

cr_extract <- function(dataset,
                       worksheet,
                       row,
                       column,
                       newname = 'newvar') {
  # Check that dataset has 5 columns, which all cost report alpha and nmrc
  # datasets do
  if (ncol(dataset) != 5) {
    stop("Dataset is not the expected size. Please only feed alpha or nmrc datasets")
  }
  
  # iteratively subset the main dataset based on worksheet, row, and column
  # Functions are stored in their own R files
  worksheet_subset <- subset_worksheet(dataset, worksheet)
  
  row_subset <- subset_row(worksheet_subset, row)
  
  column_subset <- subset_column(row_subset, column)
  
  # Keep 2 columns: first is record linkage (don't rename it), second is renamed
  # and the desired variable
  final <- column_subset[, c(1, 5)]
  names(final) <- c(names(final)[1], newname)
  

  if (nrow(final) == 0) {
    warning("Final result has no data. Double-check parameters or consider switching alpha/nmrc extraction dataset.")
  }
  
  return(final)
}