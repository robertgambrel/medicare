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
#' @param sector What sector is being adjusted. Currently supports: ip, op, phys, snf, hh, hospice, part_b_drugs, part_d_drugs, and other
#' @param current_year The current year (2002 - 2014)
#' @param reference_year The base period to standardize to (2002 - 2014).
#'   
#' @return A float value, current_value / (current year index / reference year index)
#'   
#' @examples 
#' # convert $100 in current inpatient spending to year 2005 dollars
#' price_deflate(100, "ip", 2014, 2005) 
#' @export

price_deflate <- function(current_value, sector, current_year, reference_year = 2007) {
  
  if (!(reference_year %in% 2002:2014 & current_year %in% 2002:2014)) {
    stop("Adjustment error: current_year and reference_year must be in the range 2002:2014.")
  }
  
  if (!sector %in% c("ip", "op", "phys", "snf", "hh", "hospice", "part_b_drugs",
                     "part_d_drugs", "other")) {
    stop('Sector error: sector choice must be in list c("ip", "op", "phys", "snf", "hh", "hospice", "part_b_drugs", "part_d_drugs", "other")')
  }
  
  
  # define our matrix of deflators, with 2002 as ref period.
  deflation_matrix <- data.frame(year = 2002:2014,
                                 ip = c(1.000000,	1.032000,	1.066056,	1.102302,
                                        1.141985,	1.180812,	1.220960,	1.260031,
                                        1.285231,	1.313506,	1.338463,	1.362555,	
                                        1.387081),
                                 phys = c(1.000000,	1.014000,	1.052532,	1.074635,
                                          1.076784,	1.061709,	1.065956,	1.083012,
                                          1.110087,	1.120078,	1.106637,	1.105530,
                                          1.111058),
                                 snf = c(1.000000, 1.027000, 1.057297, 1.087694,
                                         1.121412, 1.156737, 1.195198, 1.222389,
                                         1.219333, 1.245549,	1.267034,	1.288257,
                                         1.307259),
                                 hh = c(1.000000,	1.024000,	1.051648,	1.075836,
                                        1.075836,	1.111338,	1.144679,	1.177874,
                                        1.201432,	1.214648,	1.231653,	1.247664,
                                        1.234564),
                                 hospice = c(1.000000, 1.034750, 1.069673,
                                             1.106042, 1.146136, 1.184818,	
                                             1.224805, 1.262162, 1.281094,
                                             1.306396, 1.336116, 1.357828,
                                             1.382269),
                                 op = c(1.000000,	1.035000,	1.070190,	1.105506,
                                        1.146410,	1.185388,	1.224506,	1.268588,
                                        1.292057,	1.322420,	1.347546,	1.371802,
                                        1.395123),
                                 part_b_drugs = c(1.000000,	1.050811,	0.986210,
                                                  0.922396,	0.969161,	1.007889,
                                                  1.073409,	1.142826,	1.207616,
                                                  1.261932,	1.332079,	1.407099,
                                                  1.493809),
                                 part_d_drugs = c(1,	1.024813896,
                                                  1.060794045, 1.107630273,
                                                  1.128722084, 1.167937345,
                                                  1.185558313, 1.236826923,
                                                  1.287087469, 1.340276055,
                                                  1.365462159, 1.376826923,
                                                  1.464931762),
                                 other = c(1.000000, 1.022790, 1.050028,
                                           1.085603, 1.120623, 1.152540,
                                           1.196793, 1.192535, 1.212096,
                                           1.192535, 1.212096, 1.192535,
                                           1.212096))
  
  # current deflator factor
  current_factor <- deflation_matrix[deflation_matrix$year == current_year, sector]
  
  # reference factor
  reference_factor <- deflation_matrix[deflation_matrix$year == reference_year, sector]
  
  # return the deflated value
  return(current_value / (current_factor / reference_factor))
}

