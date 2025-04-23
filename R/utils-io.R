#' **Read DESCRIPTION content**
#'
#' @noRd

read_descr_file <- function() {
  check_for_descr_file()

  path <- path_proj()

  col_names <- colnames(read.dcf(file.path(path, "DESCRIPTION")))

  descr <- read.dcf(file.path(path, "DESCRIPTION"), keep.white = col_names)

  if (nrow(descr) != 1) {
    stop("Malformed 'DESCRIPTION' file", call. = FALSE)
  }

  as.data.frame(descr, stringsAsFactors = FALSE)
}


#' **Write DESCRIPTION (erase content)**
#'
#' @noRd

write_descr_file <- function(descr_file) {
  check_for_descr_file()

  path <- path_proj()

  write.dcf(
    descr_file,
    file = file.path(path, "DESCRIPTION"),
    indent = 4,
    width = 80,
    keep.white = colnames(descr_file)
  )

  invisible(NULL)
}
