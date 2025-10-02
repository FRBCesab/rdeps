#' Add to the .rdepsignore file
#'
#' @description
#' This function adds files/folders to the `.rdepsignore` file. The
#' `.rdepsignore` file is used by `rdeps` to ignore files or folders when
#' detecting dependencies. This plain text file contains a list of files/folders
#' to be ignored.
#'
#' This file will be created if it does not exist and will be added to the
#' `.Rbuildignore` file.
#'
#' @param x A `character` vector. One or several file/folder names to be added
#'   to the `.rdepsignore`. This argument is mandatory.
#'
#' @return No return value.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' add_to_rdepsignore("analyses/misc")
#' add_to_rdepsignore("analyses/old_scripts.R")
#' }

add_to_rdepsignore <- function(x) {
  if (missing(x)) {
    stop("Argument 'x' is required")
  }

  if (is.null(x)) {
    stop("Argument 'x' is required")
  }

  if (!is.character(x)) {
    stop("Argument 'x' must be a character")
  }

  path <- file.path(path_proj(), ".rdepsignore")

  ## Create new file (if missing) ----

  if (!file.exists(path)) {
    invisible(file.create(path))

    msg_done("Creating", msg_value(".rdepsignore"), "file")
  }

  add_rdepsignore_to_rbuildignore()

  ## Escape files ----

  x <- gsub("/$", "", x)

  ## Add files/folders to .Rbuildignore ----

  rdeps_ignore <- readLines(path)

  x <- x[!(x %in% rdeps_ignore)]

  if (length(x)) {
    rdeps_ignore <- c(rdeps_ignore, x)

    writeLines(rdeps_ignore, con = path)

    msg_done(
      "Adding",
      msg_value(x),
      "to",
      msg_value(".rdepsignore"),
      "file"
    )
  } else {
    msg_done("Nothing to do")
  }

  invisible(NULL)
}
