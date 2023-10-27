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



#' **Extract and clean list of packages in NAMESPACE**
#' 
#' Detect dependencies as `import(pkg)` and `importFrom(pkg,fun)`.
#' 
#' @noRd

get_deps_in_namespace <- function() {
  
  check_for_descr_file()
  
  path <- path_proj()
  
  if (file.exists(file.path(path, "NAMESPACE"))) {
    
    ui_done("Screening {ui_value('NAMESPACE')} file")
    
    namespace <- readLines(con = file.path(path, "NAMESPACE"), warn = FALSE)
    namespace <- namespace[grep("^\\s{0,}import", namespace)]
    
    deps <- gsub("importFrom\\s{0,}\\(|import\\s{0,}\\(|\\)", "", namespace)
    deps <- gsub("\\s+", " ", deps)
    deps <- trimws(deps)
    
    if (length(deps) == 0) {
      
      deps <- NULL
      
    } else {
      
      deps <- strsplit(deps, "\\s{0,},\\s{0,}")
      deps <- lapply(deps, function(x) x[1])
      deps <- sort(unique(unlist(deps)))
    }
    
  } else {
    
    ui_oops("No {ui_value('NAMESPACE')} file found")
    
    deps <- NULL
  }
  
  deps
}
