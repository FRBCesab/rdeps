#' **Check if arguments are logical of length 1**
#' 
#' @param ... one or several arguments
#' 
#' @noRd

stop_if_not_logical <- function(...) {
  
  args_values <- list(...)
  names(args_values) <- as.list(match.call())[-1]
  
  if (length(args_values)) {
    
    is_logical <- unlist(lapply(args_values, length))
    
    if (any(is_logical != 1)) {
      stop("Argument '", names(is_logical[is_logical != 1])[1], 
           "' must be a logical of length 1", call. = FALSE)
    }
    
    is_logical <- unlist(lapply(args_values, is.logical))
    
    if (any(!is_logical)) {
      stop("Argument '", names(is_logical[!is_logical])[1], 
           "' must be a logical of length 1", call. = FALSE)
    }
  }
  
  invisible(NULL)
}
