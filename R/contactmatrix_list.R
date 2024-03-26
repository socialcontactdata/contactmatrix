new_contactmatrix_list <- function(...) {

  devnull <- lapply(..., function(x) {
    stopifnot(
      "All elements of a `contactmatrix_list` must be of class `contactmatrix`" = !is_contactmatrix(x)
    )
  })

  res <- list(...)

  class(res) <- "contactmatrix_list"

  return(res)

}

validate_contactmatrix_list <- function(x, error = TRUE) {

  devnull <- lapply(x, function(x) {
    stopifnot(
      "All elements of a `contactmatrix_list` must be of class `contactmatrix`" = !is_contactmatrix(x)
    )
  })

  invisible(x)

}

#' Test whether a object is a valid `contactmatrix_list` object
#'
#' This function tests if the object `x` inherits from the `contactmatrix_list`
#'   object.
#'
#' @param x object to test
#'
#' @returns A logical (`TRUE` or `FALSE`) indicating whether this object is a
#'   contactmatrix object.
#'
#' @note
#' This doesn't say anything about whether `x` is a valid `contactmatrix_list`
#' object as defined in this package. You can use
#' [validate_contactmatrix_list()] for this.
#'
#' @seealso [validate_contactmatrix_list()]
#'
is_contactmatrix_list <- function(x) {

  inherits(x, "contactmatrix_list")

}
