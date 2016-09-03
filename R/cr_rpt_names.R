#' Provide names for Cost Report "Report Table" data
#' 
#' @return A list of names for the cost report Report Table
#' 
#' @examples 
#' 
#' # get the list
#' cr_rpt_names()
#' 

cr_rpt_names <- function() {
  return(c("rpt_rec_num",
           "prvdr_ctrl_type_cd",
           "prvdr_num",
           "npi",
           "rpt_stus_cd",
           "fy_bgn_dt",
           "fy_end_dt",
           "proc_dt",
           "initl_rpt_sw",
           "last_rpt_sw",
           "trnsmtl_num",
           "fi_num",
           "adr_vndr_cd",
           "fi_creat_dt",
           "util_cd",
           "npr_dt",
           "spec_ind",
           "fi_rcpt_dt"))
}
