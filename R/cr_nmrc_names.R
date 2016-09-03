#' Provide names for Cost Report "Numeric Table" data
#' 
#' @return A list of names for the cost report Numeric Table
#' 
#' 
#' @examples 
#' 
#' # get the list
#' cr_nmrc_names()

cr_nmrc_names <- function() {
  return(c("rpt_rec_num",
           "wksht_cd",
           "line_num",
           "clmn_num",
           "itm_val_num"))
}
