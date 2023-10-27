#' **Get project root path**
#' 
#' @noRd

path_proj <- function() usethis::proj_get()



#' **Check if a DESCRIPTION file exists**
#' 
#' @noRd

check_for_descr_file <- function() {
  
  path <- path_proj()
  
  if (!file.exists(file.path(path, "DESCRIPTION"))) {
    stop("No 'DESCRIPTION' file found", call. = FALSE)
  }
  
  invisible(NULL)
}



#' **Get project name**
#' 
#' @noRd

get_package_name <- function() {
  
  path <- path_proj()
  
  basename(path)
}



#' **Get `roxygen2` package version**
#' 
#' @noRd

get_roxygen2_version <- function() {
  
  if (!length(find.package("roxygen2", quiet = TRUE))) {
    stop("The package 'roxygen2' is not install", call. = FALSE)
  }
  
  as.character(utils::packageVersion("roxygen2"))
}
