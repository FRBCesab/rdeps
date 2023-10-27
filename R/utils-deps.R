#' **Detect dependencies in Depends field of DESCRIPTION**
#' 
#' @noRd

get_deps_in_depends <- function() {
  
  descr <- read_descr_file()
  
  if (!is.null(descr$"Depends")) {
    
    deps <- get_deps_in_field(descr$"Depends")
    
  } else {
    
    deps <- NULL
  }
  
  deps
}



#' **Detect dependencies in Imports field of DESCRIPTION**
#' 
#' @noRd

get_deps_in_imports <- function() {
  
  descr <- read_descr_file()
  
  if (!is.null(descr$"Imports")) {
    
    deps <- get_deps_in_field(descr$"Imports")
    
  } else {
    
    deps <- NULL
  }
  
  deps
}



#' **Detect dependencies in Suggests field of DESCRIPTION**
#' 
#' @noRd

get_deps_in_suggests <- function() {
  
  descr <- read_descr_file()
  
  if (!is.null(descr$"Suggests")) {
    
    deps <- get_deps_in_field(descr$"Suggests")
    
  } else {
    
    deps <- NULL
  }
  
  deps
}



#' **Remove R minimum version from list of packages**
#' 
#' @noRd

remove_r_min_version <- function(dependencies) {
  
  r_version <- grep("^R\\s{0,}\\(", dependencies)
  
  if (length(r_version)) {
    dependencies <- dependencies[-r_version]
  }
  
  dependencies
}



#' **Extract and clean list of packages in a particular DESCRIPTION field**
#' 
#' @noRd

get_deps_in_field <- function(field) {
  
  deps <- unlist(strsplit(field, "\n\\s{0,}|,|,\\s{0,}"))

  deps <- gsub("\\(\\s{0,}", " (", deps)
  deps <- gsub("\\s{0,}\\)", ")", deps)
  deps <- gsub("=", "= ", deps)
  deps <- gsub(">", "> ", deps)
  deps <- gsub("<", "< ", deps)
  
  deps <- gsub("\\s{0,}\\)", ")", deps)
  deps <- gsub("\\s+", " ", deps)
  deps <- trimws(deps)
  
  deps <- gsub("< =", "<=", deps)
  deps <- gsub("> =", ">=", deps)
  deps <- gsub("= =", "==", deps)
  
  deps <- deps[!(deps == "")]
  
  remove_r_min_version(deps)
}
