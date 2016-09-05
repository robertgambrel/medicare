#' Provide names for Cost Report "Alpha Table" data
#' 
#' @return A list of names for the cost report Alpha Table
#' 
#' @examples
#' 
#' # get the list
#' cr_nmrc_names()
#' 
#' @export

cr_alpha_names <- function() {
  return(c("rpt_rec_num",
           "wksht_cd",
           "line_num",
           "clmn_num",
           "itm_alphanmrc_itm_txt"))
}
