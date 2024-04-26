#' 
#' @title Remove Unwanted Spaces
#' 
#' @param x a character or vector
#'
#' @return the character `x` without trailing or double spaces
#'  
#' @keywords internal
#'
#' @noRd
#' 
.squish <- function (x) {
  x <- gsub("\\s\\s+", " ", as.character(x), perl = TRUE)
  x <- gsub("^ | $", "", x, perl = TRUE)
  return(x)
}

#' 
#' @title Build Organism Name
#' 
#' @description Combine different columns with species name
#'   information (i.e. genus, epiteth, infra-epiteth, author) into a
#'   single organism name
#' 
#' @param x the data frame with the taxonomic information
#' @param col.names the name of the columns containing the information
#'   to be combined in the desired order of output
#'
#' @return a vector of names combining the information in 'col.names' 
#'  
#' @keywords internal
#' 
#' @noRd
#' 
.buildName <- function(x, col.names = c("genus", "species")) {
  
  if (any(!col.names %in% colnames(x)))
    stop("One or more names in 'col.names' were not found in 'x'")
  
  cols <- names(x)[match(col.names, names(x), nomatch = 0)]
  
  if (length(cols) > 1) {
    
    organismName <- do.call(paste, x[, cols])
    organismName[organismName %in% 
                   c("NA NA", "NA NA NA",
                     "NULL NULL", "NULL NULL NULL")] <- NA_character_
    organismName <- gsub(" NA NA$| NULL NULL$", "", organismName, 
                         perl = TRUE)
    organismName <- gsub(" NA$| NULL$", "", organismName, 
                         perl = TRUE)
    organismName <- gsub(" NA ", " ", organismName, 
                         fixed = TRUE)
    organismName <- .squish(organismName)
    return(organismName)
  } else {
    return(.squish(x[[cols]]))
  }    
}


#' 
#' @title Read File in Help
#' 
#' @param file a path to the file with the script to be read
#' @param text a character or vector of additional text to be added to
#'   the tag
#' @param tag one of Roxygen's documentation tag. Defaults to 'note'.
#'  
#' @keywords internal
#'
#' @noRd
#' 
.readScript <- function(file = NULL, text = "", tag = "note") {
  lns <- readLines(file)
  lns <- paste(sprintf("\\code{%s}", lns), collapse = "; ")
  return(paste("\\", tag, "{", text, "\n", lns, ".}", sep = ""))
}

#' 
#' @title Get Date of Last Update
#' 
#' @param url a url to the parent directory where the data is
#'   available
#' @param source a character. The name of the source (i.e. wfo, wcvp,
#'   gbif, gadm, etc.)
#' @param pattern a character. The pattern to help finding the last
#'   updated information within the url content.
#'  
#' @keywords internal
#'
#' @noRd
#' 
.getUpdates <- function (url = NULL, source = NULL, pattern = NULL) {
  
  web_url <- httr::GET(url)
  content <- httr::content(web_url)
  node <- rvest::html_element(content, "pre")
  
  if(is.na(node)) {
    
    text <- rvest::html_text(content)
    lines <- strsplit(text, " ")[[1]]
    current <- lines[grepl(pattern, lines)]
    current_date <- 
      sapply(strsplit(current, " "), function(x) x[grepl("20[0-9][0-9]", x)])
    current_date <- gsub(pattern, "", current_date)
    
    return(current_date)

  } else {
    if(grepl("<br>", as.character(node))) {
      text <- rvest::html_text2(node)
    } else {
      text <- rvest::html_text(node)
    }
    
    lines <- strsplit(text, "\n")[[1]]
    current <- lines[grepl(pattern, lines)]
    current_date <- 
      sapply(strsplit(current, " "), function(x) x[grepl("20[0-9][0-9]", x)])
    current_date <- gsub(pattern, "", current_date)
    
    return(current_date)
  }
}
