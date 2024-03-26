#' Constructor for a `contactmatrix` object
#'
#' @param from Character vector, or list of character vectors in the case of
#'    multiple groupings, indicating the characteristics of each
#' @param to
#' @param value
#' @param fill Numeric value to use for contacts non-defined in the `from`,
#'   `to`, `value` argument triplet. Defaults to `0`; i.e., no contact.
#'
#' @details
#' - `from`, `to` and `value` must have the same length, as
#'
#' @export
#'
#' @examples
#' # Usual case; single grouping by age
#' new_contactmatrix(
#'   from  = c("00_05", "05_10", "05_10"),
#'   to    = c("00_05", "10_15", "15_20"),
#'   value = c(0.32   , 0.46   , 0.72   )
#' )
#'
#' # Multiple groupings; gender & case
#' dat <- data.frame(
#'   age    = c("young", "young", "old"),
#'   gender = c("male", "female", "female"),
#'   age    = c("old", "old", "young"),
#'   gender = c("female", "female", "female"),
#'   value  = c(1, 2, 2)
#' )
#'
#' new_contactmatrix(
#'   from  = dat[, c(1, 2)],
#'   to    = dat[, c(3, 4)],
#'   value = dat[, "value"]
#' )
#'
new_contactmatrix <- function(
    from,
    to,
    value,
    fill = 0,
    symmetric = NA
  ) {

  # from checks
  if (!is.list(from))
    from <- list(from)

  length_from <- length(from)
  if (!length(from))
    stop("`from` cannot be empty.")

  nrows_from <- unique(lengths(from))
  if (length(nrows_from) > 1L)
    stop("all variables in `from` must have the same length.")

  # to checks
  if (!is.list(to))
    to <- list(to)
  length_to <- length(to)
  if (length_from != length_to)
    stop("`from` and `to` must have equal length.")

  nrows_to <- unique(lengths(to))
  if (length(nrows_to) > 1L || nrows_to != nrows_from)
    stop("all variables in `to` must have the same length as those in `from`.")

  # value checking
  if(!is.numeric(value))
    stop("`value` must be <numeric>.")
  if (length(value) != nrows_from)
    stop("`value` must be the same length as the variables in `from`.")

  possible_traits <- Map(
    function(x1, x2) sort(unique(c(x1, x2)), method = "radix"),
    from,
    to
  )

  possible_traits <- setNames(
    c(possible_traits, possible_traits),
    paste(rep(names(possible_traits), 2), rep(c("from", "to"), each = length_from), sep = "_")
  )

  x <- array(
    data = fill,
    dim = lengths(possible_traits),
    dimnames = possible_traits
  )

  x[as.matrix(list2DF(c(from, to)))] <- value

  class(x) <- c("contact_matrix", "array")
  attr(x, "symmetric") <- symmetric

  return(x)

}

validate_contactmatrix <- function(x, error = TRUE) {

  is.logical(attr(x, "symmetric"))

}

as_contactmatrix <- function(x, ...) {

  UseMethod()

}

as_contactmatrix.default <- function(x, ...) {

  stop(
    "`as_contactmatrix()` method not defined for this class of object",
    call. = FALSE
  )

}

#' Test whether a object is a valid `contactmatrix` object
#'
#' This function tests if the object `x` inherits from the `contactmatrix`
#'   object.
#'
#' @param x object to test
#'
#' @returns A logical (`TRUE` or `FALSE`) indicating whether this object is a
#'   contactmatrix object.
#'
#' @note
#' This doesn't say anything about whether `x` is a valid `contactmatrix` object
#' as defined in this package. You can use [validate_contactmatrix()] for this.
#'
#' @seealso [validate_contactmatrix()]
#'
is_contactmatrix <- function(x) {

  inherits(x, "contactmatrix")

}

