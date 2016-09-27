#' Process a Provider of Services Record Layour file to extract variable names
#' 
#' This function takes a Provider of Services Record Layout file (in .txt form) 
#' and parses it to extract the descriptive variable names instead of generic 
#' ones. For example, the 2006 file variable PROV0085, which is the name of the 
#' variable in the raw dataset downloaded from CMS, has a more descriptive name 
#' in the layout file: CATEGORY-SUBTYPE-IND.
#' 
#' This uses regular expressions to find variable names. It works with years 
#' 2000-2010. Later years seem to have descriptive names already, though they 
#' aren't necessarily identical across years (nor do they match the names 
#' produced here). This code can be run to produce variable names fresh, but
#' pre-computed variable names can also be accessed by \code{names_pos_20XX()}
#' functions also in this package.
#' 
#' @param layout_file The file location of the layout file
#' @param data_file The year's data file
#' @return A vector of names, ordered to match the corresponding year's data 
#'   file
#'   
#' @examples 
#' \dontrun{
#' pos_names_extract("pos_2006_layout.txt", pos_2006_data)
#' }
#' 
#' 
#' @export

pos_names_extract <- function(layout_file, data_file) {
  
  layout <- readLines(layout_file)
  
  # Pull lines with potentially matching variable names. In my experience, they 
  # contain any of these patterns: PROV#### (exactly 4 numbers after PROV), 
  # FIP@@@(@@) (3,5 letters after FIP), SSSAM@@@(@@) (3,5 letters after SSAM). 

  oldcols <- grep("PROV[[:digit:]]{4}|FIP[[:alpha:]]{3,5}|SSAM[[:alpha:]]{3,5}", 
                  layout, value = T)
  
  # check: make sure the file is a layout file
  if (length(oldcols) == 0) {
    stop("Chosen layout_file had no generic variable names of PROV----, FIP-----, or SSAM-----. Make sure you use a layout file.")
  }
  
  ## Some extra lines match the FIP and SSAM above (they're descriptor lines).
  ## Drop them for being too short:
  oldcols<-subset(oldcols, nchar(oldcols)>50)
  
  # From those lines, only keep the variable names. Could probably do this with
  # a substring, since the cols are fixed width, but keep doing regex to be safe
  
  match <- regexpr("PROV[[:digit:]]{4}|FIP[[:alpha:]]{3,5}|SSAM[[:alpha:]]{3,5}", oldcols)
  
  old_names <- as.character(regmatches(oldcols, match))
  
  # Subset new variable name (indicated by COBOL NAME in the txt file)
  newcols <- grep("COBOL NAME", layout, value = T)
  
  # these entries follow a pattern: all on one, all of the format
  # "     COBOL NAME: XXXXXXXXXXX"
  # So drop the leading spaces, COBOL NAME:, and you're set
  newcols <- gsub("COBOL NAME:", "", newcols)
  newcols <- gsub("[[:space:]]", "", newcols)
  
  # Cobol uses dashes in names, convert to underscores to play nicely with R
  newcols <- gsub("-", "_", newcols)
  new_names <- as.character(newcols)
  
  # they should be the same length. If not, raise an error
  if (length(old_names) != length(new_names)) {
    stop("Problem parsing layout: count of original names and descriptive names did not match.")
  }
  
  # combine old and new (they're sorted in the data, so OK to just cbind them)
  colmatches <- as.data.frame(cbind(old_names, new_names), stringsAsFactors = F)
  
  # Frequently, the layout lists the same variable for each sector that reports
  # that variable (so provider number is in the layout file for hospitals,
  # HHA's, SNF's, etc. Drop duplicates)
  colmatches <- unique(colmatches)
  
  ## Find the order of the names in the POS data file, make sure that new set is
  ## ordered the same way. In all likelihood, this won't change anything, but
  ## best to be safe
  ordering <- toupper(names(data_file))
  
  # reorder the variable so that the new names are in the same name as the old
  # names in the data frame
  colmatches <- colmatches[match(ordering, colmatches$oldcols), ]
  
  return(colmatches)
  
}