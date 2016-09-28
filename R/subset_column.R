#' Subset the desired dataset, based on a column code.
#' 
#' This function is not standalone - it is called from the cr_extract function.
#' 
#' @param dataset The name of a cost report alpha or numeric dataset
#' @param column The column of the workheet
#'   
#' @return A subset of the provided dataset, subset to only having columns of the
#'   correct value


subset_column <- function(dataset, column) {
  
  # check that column is entered correctly
  if (nchar(column) < 3 | is.na(column)) {
    warning("Column number has less than 3 digits. This can produce unexpected output; please ensure that you convert to 100's (i.e. column 1 -> '100', column 1.1 -> '101'")
  }
  
  # subset the data, being flexible with how the column number is entered,  since
  # the cost reports are inconsistent themselves
  no_zero_column <- gsub("^0*", "", column)
  one_zero_column <- paste0("0", no_zero_column)
  two_zero_column <- paste0("00", no_zero_column)
  
  # sometimes column numbers are numeric instead of character. Try to convert them to
  # numeric, and if it fails, silently return NA
  
  numeric_column <- tryCatch({as.numeric(column)}, 
                          error = function(x) return(NA), 
                          warning = function(x) return(NA))
  
  data_subset <- dataset[dataset[, 4] %in% list(no_zero_column, 
                                         one_zero_column,
                                         two_zero_column, 
                                         numeric_column) &
                     !is.na(dataset[, 4]), ]
  

  if (nrow(data_subset) == 0) {
    warning("No data found with specified column number.")
  }
  
  return(data_subset)
}