#' @noRd

msg_done <- function(..., indent = "") {
  x <- paste(...)
  x <- msg_bullet(x, indent = indent, cli::col_green(cli::symbol$"tick"))
  cli::cat_line(x)

  invisible(NULL)
}


#' @noRd

msg_oops <- function(..., indent = "") {
  x <- paste(...)
  x <- msg_bullet(x, indent = indent, cli::col_red(cli::symbol$"cross"))
  cli::cat_line(x)

  invisible(NULL)
}


#' @noRd

msg_info <- function(..., indent = "") {
  x <- paste(...)
  x <- msg_bullet(x, indent = indent, cli::col_blue("(*)"))
  cli::cat_line(x)

  invisible(NULL)
}


#' @noRd

msg_line <- function(..., indent = "") {
  x <- paste(...)
  x <- paste0(indent, x)
  cli::cat_line(x)

  invisible(NULL)
}


#' @noRd

msg_value <- function(...) {
  x <- unlist(list(...))

  if (is.character(x)) {
    x <- encodeString(x, quote = "'")
  }

  invisible(paste0(cli::col_blue(x), collapse = ", "))
}


#' @noRd

msg_code <- function(...) {
  x <- unlist(list(...))

  if (is.character(x)) {
    x <- encodeString(x, quote = "`")
  }

  invisible(paste0(cli::col_silver(x), collapse = ", "))
}


#' @noRd

msg_bullet <- function(x, indent = "", bullet = cli::symbol$"bullet") {
  bullet <- paste0(indent, bullet, " ")
  msg_indent(x, bullet, "  ")
}


#' @noRd

msg_indent <- function(x, first = "  ", indent = first) {
  x <- gsub("\n\\s", "\n", x)
  x <- gsub("\n", paste0("\n", indent), x)
  paste0(first, x)
}
