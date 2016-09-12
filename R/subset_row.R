#' Subset the desired dataset, based on a row code.
#' 
#' This function is not standalone - it is called from the cr_extract function.
#' 
#' @param dataset The name of a cost report alpha or numeric dataset
#' @param row The number of the row
#'   
#' @return A subset of the provided dataset, subset to only having rows of the
#'   correct value
#'   
#' @examples 
#' 
#' alpha_data <- hospiceALPHA
#' row_100 <- subset_row(alpha_data, 100)
#' 

subset_row <- function(dataset, row) {
  
  # check that column is entered correctly
  if (nchar(row) < 3 | is.na(row)) {
    warning("Row number has less than 3 digits. This can produce unexpected output; please ensure that you convert to 100's (i.e. column 1 -> '100', column 1.1 -> '101'")
  }
  
  # subset the data, being flexible with how the row number is entered,  since
  # the cost reports are inconsistent themselves
  no_zero_row <- gsub("^0*", "", row)
  one_zero_row <- paste0("0", no_zero_row)
  two_zero_row <- paste0("00", no_zero_row)
  
  # sometimes row numbers are numeric instead of character. Try to convert them to
  # numeric, and if it fails, silently return NA
  
  numeric_row <- tryCatch({as.numeric(row)}, 
                          error = function(x) return(NA), 
                          warning = function(x) return(NA))
  
  data_subset <- dataset[dataset[, 3] %in% list(no_zero_row, 
                                         one_zero_row,
                                         two_zero_row, 
                                         numeric_row) &
                     !is.na(dataset[, 3]), ]
  

  if (nrow(data_subset) == 0) {
    warning("No data found with specified row number.")
  }
  
  return(data_subset)
}