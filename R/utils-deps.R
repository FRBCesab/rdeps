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



#' **Extract and clean list of packages in the R folder**
#' 
#' Detect dependencies in R functions written as `foo::bar()`, `library(foo)`,
#' `library("foo")`, `library('foo')`, `require(foo)`, `require("foo")`, 
#' `require('foo')`.
#' 
#' @return A named `list` of two `character` vectors:
#'   - `depends`, packages called with `library(foo)` and `require(foo)`;
#'   - `imports`, packages called with `foo:bar()`.
#'   
#' If a package is called with `library(foo)` and `foo:bar()`, it will be added
#' only to `depends`.
#' 
#' @noRd

get_deps_in_functions <- function(directory = "R") {
  
  check_for_descr_file()
  
  path <- path_proj()
  
  
  ## No `directory` folder ----
  
  if (!dir.exists(file.path(path, directory))) {
    
    return(list("depends" = NULL,"imports" = NULL))
  }
  
  
  r_files <- list.files(path = file.path(path, directory), pattern = "\\.R$", 
                        full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
  
  
  ## No .R files in `directory` ----
  
  if (!length(r_files)) {
    
    return(list("depends" = NULL,"imports" = NULL))
  }
    
    
  ## Read R files ----
  
  content <- lapply(r_files, function(x) readLines(con = x, warn = FALSE))
  
  
  ## Remove comments ----
  
  content <- remove_comment_lines(content)
  
  
  ## Remove messages ----
  
  content <- remove_messages(content)
  
  
  ## Functions called as pkg::fun() ----
  
  deps_imports <- get_colon_syntax_deps(content)
  
  
  ## Attached Packages (library & require) ----
  
  deps_depends <- get_attached_deps(content)
  
  
  ## Remove duplicates ----
  
  pos <- which(deps_imports %in% deps_depends)
  
  if (length(pos) > 0) deps_imports <- deps_imports[-pos]
  
  
  ## Remove project name ----
  
  pos <- which(deps_depends == basename(path))
  if (length(pos) > 0) deps_depends <- deps_depends[-pos]
  
  pos <- which(deps_imports == basename(path))
  if (length(pos) > 0) deps_imports <- deps_imports[-pos]
  
  
  ## Clean objects ----
  
  if (length(deps_depends) == 0) deps_depends <- NULL
  if (length(deps_imports) == 0) deps_imports <- NULL
  
  list("depends" = deps_depends, "imports" = deps_imports)
}



#' **Detect dependencies in @examples**
#' 
#' Detect dependencies in **@examples** as `foo::bar()`, `library(foo)`, and
#' `require(foo)`.
#' 
#' @noRd

get_deps_in_examples <- function(directory = "R") { 
  
  check_for_descr_file()
  
  path <- path_proj()
  
  
  ## No `directory` folder ----
  
  if (!dir.exists(file.path(path, directory))) {
    
    return(list("depends" = NULL,"imports" = NULL))
  }
  
  
  r_files <- list.files(path = file.path(path, directory), pattern = "\\.R$", 
                        full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
  
  
  ## No .R files in `directory` ----
  
  if (!length(r_files)) {
    
    return(list("depends" = NULL,"imports" = NULL))
  }
  
  
  ## Read R files ----
    
  content <- lapply(r_files, function(x) readLines(con = x, warn = FALSE))
  
  
  ## Select roxygen2 headers ----
  
  content <- lapply(content, function(x) x[grep("^\\s{0,}#'", x)])
  
  
  ## Select @examples sections (dirty code, but working) ----
  
  ex_start <- lapply(content, function(x) grep("^\\s{0,}#'\\s{0,}@examples", x))
  ex_end   <- lapply(content, function(x) grep("#'\\s{0,}@", x))
  
  examples <- list()
  
  for (i in seq_len(length(ex_start))) {
    
    examples[[i]] <- character(0)
    
    if (length(ex_start[[i]])) {
      
      for (j in seq_len(length(ex_start[[i]]))) {
        
        pos <- ex_end[[i]][ex_end[[i]] > ex_start[[i]][j]] 
        
        if (length(pos)) {
          
          examples[[i]] <- c(
            examples[[i]],
            content[[i]][(ex_start[[i]][j] + 1):(pos[1] - 1)])
          
        } else {
          
          examples[[i]] <- c(
            examples[[i]],
            content[[i]][(ex_start[[i]][j] + 1):length(content[[i]])])
        }
      }
    }
  }
  
  content <- examples
  
  
  ## Remove comments ----
  
  content <- remove_rd_comment_lines(content)
  
  
  ## Remove messages ----
  
  content <- remove_messages(content)
  
  
  ## Functions called as pkg::fun() ----
  
  deps_imports <- get_colon_syntax_deps(content)
  
  
  ## Attached Packages (library & require) ----
  
  deps_depends <- get_attached_deps(content)
  
  
  ## Remove duplicates ----
  
  pos <- which(deps_imports %in% deps_depends)
  
  if (length(pos) > 0) deps_imports <- deps_imports[-pos]
  
  
  ## Remove project name ----
  
  pos <- which(deps_depends == basename(path))
  if (length(pos) > 0) deps_depends <- deps_depends[-pos]
  
  pos <- which(deps_imports == basename(path))
  if (length(pos) > 0) deps_imports <- deps_imports[-pos]
  
  
  ## Clean objects ----
  
  if (length(deps_depends) == 0) deps_depends <- NULL
  if (length(deps_imports) == 0) deps_imports <- NULL
  
  list("depends" = deps_depends, "imports" = deps_imports)
}



#' **Remove comment lines**
#' 
#' Remove comment lines in .R files
#' 
#' @noRd

remove_comment_lines <- function(x) {
  
  lapply(x, function(x) {
    gsub("#.*", "", x)  
  })
}



#' **Remove comment lines in @examples**
#' 
#' Remove comment lines in .R files in `@examples`
#' 
#' @noRd

remove_rd_comment_lines <- function(x) {
  
  lapply(x, function(x) {
    gsub("(#'.*)(#.*)", "\\1", x)  
  })
}



#' **Remove messages**
#' 
#' Remove messages in `stop()`, `messages()`, `cat()`, `print()`, etc.
#' 
#' @noRd

remove_messages <- function(x) {
  
  x <- lapply(x, function(x) gsub("\".{0,}\'.{0,}\'.{0,}\"", "", x))
  x <- lapply(x, function(x) gsub("\'.{0,}\".{0,}\".{0,}\'", "", x))
  x <- lapply(x, function(x) gsub("\".{0,}\\\".{0,}\\\".{0,}\"", "", x))
  x <- lapply(x, function(x) gsub("\'.{0,}\\\'.{0,}\\\'.{0,}\'", "", x))
  
  x
}



#' **Detect packages called with the double colon syntax**
#' 
#' Detect packages called with the double colon syntax (i.e. `foo::bar()`).
#' 
#' @noRd

get_colon_syntax_deps <- function(x) {
  
  pattern <- paste0("[A-Z|a-z|0-9|\\.]{1,}\\s{0,}", 
                    "::", 
                    "\\s{0,}[A-Z|a-z|0-9|\\.|_]{1,}")
  
  funs <- unlist(lapply(x, function(x) {
    unlist(stringr::str_extract_all(x, pattern))
  }))
  
  funs <- gsub("\\s", "", funs)
  
  deps <- strsplit(funs, "::")
  deps <- lapply(deps, function(x) x[1])
  deps <- sort(unique(unlist(deps)))
  
  if (length(deps) == 0) deps <- NULL
  
  deps
}



#' **Detect attached packages**
#' 
#' Detect attached packages, i.e. called by `library()` and `require()`.
#' 
#' @noRd

get_attached_deps <- function(x) {
  
  pattern <- c(paste0("library\\s{0,}\\(\\s{0,}([A-Z|a-z|0-9|\\.]{1,}|",
                      "\"\\s{0,}[A-Z|a-z|0-9|\\.]{1,}\\s{0,}\"|",
                      "\'\\s{0,}[A-Z|a-z|0-9|\\.]{1,}\\s{0,}\')"),
               paste0("require\\s{0,}\\(\\s{0,}([A-Z|a-z|0-9|\\.]{1,}|",
                      "\"\\s{0,}[A-Z|a-z|0-9|\\.]{1,}\\s{0,}\"|",
                      "\'\\s{0,}[A-Z|a-z|0-9|\\.]{1,}\\s{0,}\')"))
  pattern <- paste0(pattern, collapse = "|")
  
  deps <- unlist(lapply(x, function(x) {
    unlist(stringr::str_extract_all(x, pattern))
  }))
  
  deps <- gsub("\\s", "", deps)
  deps <- gsub("library\\(|require\\(|\"|\'", "", deps)
  
  if (length(deps) == 0) deps <- NULL
  
  deps
}
