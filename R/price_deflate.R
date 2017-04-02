#' Deflate prices within a sector, relative to a base period.
#' 
#' CMS publishes yearly final rules that detail annual price increases across 
#' various sectors of healthcare spending. In order to analyze spending
#' increases due to utilization changes, it is frequently useful to "deflate"
#' spending based on a reference period, so that observed changes are not due to
#' inflation.
#' 
#' Most sectors come from annual Federal Register Final Rules. Example:
#' \href{https://www.cms.gov/Medicare/Medicare-Fee-for-Service-Payment/PhysicianFeeSched/index.html}{Physician Fee Change Rules}.
#' 
#' Exceptions are \code{other}, which uses the general CPI deflator, and
#' \code{part_d_drugs}, which use the CPI-Pharmaceutical deflator.`
#' 
#' @param current_value The current value that is being deflated to reference-period-equivalent dollars
#' @param sector What sector is being adjusted. Currently supports: ip, op, phys, snf, hh, hospice, part_b_drugs, part_d_drugs, dme, and other
#' @param current_year The current year (2007 - 2014)
#' @param reference_year The base period to standardize to (2007 - 2014).
#'   
#' @return A float value, current_value / (current year index / reference year index)
#'   
#' @examples 
#' # convert $100 in current inpatient spending to year 2007 dollars
#' price_deflate(100, "ip", 2014, 2007) 
#' @export

price_deflate <- function(current_value, sector, current_year, reference_year = 2007) {
  
  if (!(reference_year %in% 2007:2014 & current_year %in% 2007:2014)) {
    stop("Adjustment error: current_year and reference_year must be in the range 2007:2014.")
  }
  
  if (!sector %in% c("ip", "op", "phys", "snf", "hh", "hospice", "part_b_drugs",
                     "part_d_drugs", "dme", "other")) {
    stop('Sector error: sector choice must be in list c("ip", "op", "phys", "snf", "hh", "hospice", "part_b_drugs", "part_d_drugs", "other")')
  }
  
  
  # define our matrix of deflators, with 2002 as ref period.
  deflation_matrix <- data.frame(year = 2007:2014,
                                 ip = c(1.00000, 1.03375, 1.06709, 1.09083,
                                        1.12028, 1.14241, 1.16440, 1.18798),
                                 phys = c(1.00000, 1.00500, 1.01606, 1.02926,
                                          1.03853, 1.03853, 1.03853, 1.04372),
                                 snf = c(1.00000, 1.03325, 1.06528, 1.08898,
                                         1.10750, 1.12660, 1.14547, 1.16237),
                                 hh = c(1.00000, 1.03000, 1.05987, 1.08107,
                                        1.09296, 1.10826, 1.12267, 1.14849),
                                 hospice = c(1.00000, 1.03375, 1.06709, 1.08870,
                                             1.11020, 1.13546, 1.15391, 1.17468),
                                 op = c(1.00000, 1.03300, 1.07019, 1.08999, 
                                        1.11560, 1.14628, 1.17322, 1.19962),
                                 part_b_drugs = c(1.00000, 1.06501, 1.13388, 1.19816,
                                                  1.12505, 1.32165, 1.39609, 1.48212),
                                 part_d_drugs = c(1.00000, 1.01509, 1.05898, 1.10202,
                                                  1.14756, 1.16912, 1.17885, 1.25429),
                                 dme = c(1.00000, 1.00000, 1.00230, 0.98800, 
                                         0.99840, 1.02236, 1.03054, 1.04085),
                                 other = c(1.00000, 1.02872, 1.05675, 1.07798, 
                                           1.09896, 1.11940, 1.13785, 1.16052))
  
  # current deflator factor
  current_factor <- deflation_matrix[deflation_matrix$year == current_year, sector]
  
  # reference factor
  reference_factor <- deflation_matrix[deflation_matrix$year == reference_year, sector]
  
  # return the deflated value
  return(current_value / (current_factor / reference_factor))
}

