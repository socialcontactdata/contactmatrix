#' Constructor for a `contactmatrix_list` object
#'
#' @param ... `contactmatrix` objects to include in the list. All elements
#' must have the same groupings.
#'
#' @returns
#' A list of `contactmatrix` objects, of class `contactmatrix_list`.
#'
#' @export
#'
#' @examples
#' cm1 <- new_contactmatrix(
#'   from  = c("00_05", "05_10", "05_10"),
#'   to    = c("00_05", "10_15", "15_20"),
#'   value = c(0.32   , 0.46   , 0.72   )
#' )
#'
#' cm2 <- new_contactmatrix(
#'   from  = c("05_10", "05_10", "05_10"),
#'   to    = c("00_05", "10_15", "15_20"),
#'   value = c(0.27   , 0.09   , 0.32   )
#' )
#'
#' cml <- new_contactmatrix_list(cm1, cm2)
new_contactmatrix_list <- function(...) {

  res <- list(...)

  class(res) <- "contactmatrix_list"

  assert_contactmatrix_list(res)

  return(res)

}

#' Assert whether a object is a **valid** `contactmatrix_list` object
#'
#' This function asserts that the object `x` is a valid `contactmatrix_list`
#'   object.
#'
#' @param x object to test
#'
#' @returns An error if `x` is not a valid `contactmatrix_list` object and `x`
#' invisibly otherwise.
#'
#' @export
#'
#' @seealso [test_contactmatrix_list()]
assert_contactmatrix_list <- function(x) {

  if (!test_contactmatrix_list(x)) {
    stop(
      "All elements of a contactmatrix_list must be contact_matrix ",
      "with the same groupings, divided in the same groups",
      call. = FALSE
    )
  }

  return(invisible(x))

}

#' Test whether a object is a **valid** `contactmatrix_list` object
#'
#' This function tests if the object `x` is a valid `contactmatrix_list`
#'   object.
#'
#' @param x object to test
#'
#' @returns A logical (`TRUE` or `FALSE`) indicating whether this object is a
#' valid `contactmatrix_list` object.
#'
#' @export
#'
#' @seealso [test_contactmatrix_list()]
test_contactmatrix_list <- function(x) {

  all_cm <- all(vapply(x, is_contactmatrix, logical(1)))
  groupings <- lapply(x, dimnames)
  is_cml <- is_contactmatrix_list(x)

  return(is_cml && all_cm && length(unique(groupings)) == 1)

}

#' Test whether a object is a `contactmatrix_list` object
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
#' [assert_contactmatrix_list()] for this.
#'
#' @export
#'
#' @seealso [assert_contactmatrix_list()], [test_contactmatrix_list()]
#'
is_contactmatrix_list <- function(x) {

  inherits(x, "contactmatrix_list")

}
