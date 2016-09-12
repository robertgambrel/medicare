#' Subset the desired dataset, based on a worksheet code.
#' 
#' This function is not standalone - it is called from the cr_extract function.
#' 
#' @param dataset The name of a cost report alpha or numeric dataset
#' @param worksheet The name of the workheet, converted to 7-character format
#'   
#' @return A subset of the provided dataset, subset to only having worksheets of
#'   the correct value
#'   
#' @examples 
#' 
#' alpha_data <- hospiceALPHA
#' sheet_s1 <- subset_worksheet(alpha_data, "S100000")
#' 

subset_worksheet <- function(dataset, worksheet) {
  
  data_subset <- dataset[dataset[, 2] == toupper(worksheet), ]
  
  if (nrow(data_subset) == 0) {
    warning(paste0("No data found with worksheet ", worksheet,
                   ". Double-check and ensure it matches Medicare's 7-character style."))
  }
  
  return(data_subset)
}