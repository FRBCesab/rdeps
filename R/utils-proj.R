#' **Get project root path**
#'
#' @noRd

path_proj <- function() {
  is_descr_file <- file.exists(file.path(getwd(), "DESCRIPTION"))
  is_here_file <- file.exists(file.path(getwd(), ".here"))
  is_git_folder <- dir.exists(file.path(getwd(), ".git"))

  is_rproj_file <- ifelse(
    length(list.files(getwd(), pattern = "\\.Rproj")) > 0,
    TRUE,
    FALSE
  )

  if ((is_descr_file + is_here_file + is_git_folder + is_rproj_file) == 0) {
    stop(
      "It appears that the current working directory is not a valid ",
      "project or package. Read more at ",
      "'https://usethis.r-lib.org/reference/proj_utils.html'",
      call. = FALSE
    )
  }

  getwd()
}


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


#' **Add `.rdepsignore` to `.Rbuildignore`**
#'
#' @noRd

add_rdepsignore_to_rbuildignore <- function() {
  path <- file.path(path_proj(), ".Rbuildignore")
  if (!file.exists(path)) {
    invisible(file.create(path))

    msg_done("Creating", msg_value(".Rbuildignore"), "file")
  }

  x <- "^\\.rdepsignore$"

  rbuild_ignore <- readLines(path)

  x <- x[!(x %in% rbuild_ignore)]

  if (length(x)) {
    rbuild_ignore <- c(rbuild_ignore, x)

    writeLines(rbuild_ignore, con = path)

    msg_done(
      "Adding",
      msg_value(x),
      "to",
      msg_value(".Rbuildignore"),
      "file"
    )
  }

  invisible(NULL)
}
