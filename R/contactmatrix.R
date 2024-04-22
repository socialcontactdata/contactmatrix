#' Constructor for a `contactmatrix` object
#'
#' @param from Character vector, or list of named character vectors in the case
#'    of multiple groupings, indicating the characteristics of the group
#'    contacting.
#' @param to Character vector, or list of named character vectors in the case
#'    of multiple groupings, indicating the characteristics of the group
#'    contacted.
#' @param value Value of the contact between individual in groups `from` and
#'    group `to`
#' @param fill Numeric value to use for contacts non-defined in the `from`,
#'   `to`, `value` argument triplet. Defaults to `0`; i.e., no contact.
#' @param symmetric Logical value (default to `NA`) indicating whether the
#'   resulting `[`contactmatrix` is symmetric, i.e.,  $c_{ij}N_i == c_{ji}N_j$
#'   where $i$ is a group from `from`, $j$ a group from `to`, $c$ their
#'   corresponding contact `value` and $N$ the group size.
#'
#' @details
#' - `from`, `to` and `value` must have the same length
#'
#' @returns
#' An array (a matrix if only 1 grouping = 2 dimensions) of subclass
#' `contactmatrix`.
#' The array has $2n$ dimensions where $n$ is the number of groupings.
#'
#' @importFrom stats setNames
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
#' cm3d <- new_contactmatrix(
#'   from  = list(
#'     age = c("young", "young", "old"),
#'     gender = c("male", "female", "female")
#'   ),
#'   to    = list(
#'     age    = c("old", "old", "young"),
#'     gender = c("female", "female", "female")
#'   ),
#'   value = c(1, 2, 2)
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
  if (!is.numeric(value))
    stop("`value` must be <numeric>.")
  if (length(value) != nrows_from)
    stop("`value` must be the same length as the variables in `from`.")

  possible_traits <- Map(
    function(x1, x2) sort(unique(c(x1, x2)), method = "radix"),
    from,
    to
  )

  possible_interactions <- setNames(
    c(possible_traits, possible_traits),
    paste(
      rep(names(possible_traits), 2),
      rep(c("from", "to"), each = length_from),
      sep = "_"
    )
  )

  x <- array(
    data = fill,
    dim = lengths(possible_interactions),
    dimnames = possible_interactions
  )

  x[as.matrix(list2DF(c(from, to)))] <- value

  class(x) <- c("contactmatrix", class(x))
  attr(x, "symmetric") <- symmetric
  attr(x, "groupings") <- simplify_list(possible_traits)

  return(x)

}

#' Assert whether a object is a **valid** `contactmatrix` object
#'
#' This function asserts that the object `x` is a valid `contactmatrix`
#'   object.
#'
#' @param x object to test
#'
#' @returns An error if `x` is not a valid `contactmatrix` object and `x`
#' invisibly otherwise.
#'
#' @note Checks:
#' - `x` inherits from the `array` class
#' - `x` inherits from the `contactmatrix` class
#' - `x` contains only numeric values
#' - `x` is an hypercube, i.e., number of rows and number of columns are equal
#' - `x` has a `symmetric` attribute of class `logical`
#'
#' @export
#'
#' @seealso [test_contactmatrix_list()]
assert_contactmatrix <- function(x) {

  if (!test_contactmatrix(x)) {
    stop(
      "Object is not a valid `contactmatrix` object.",
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
#' @inherit assert_contactmatrix note
#'
#' @export
#'
#' @seealso [test_contactmatrix_list()]
test_contactmatrix <- function(x) {

  return(
    inherits(x, "array") &&
      is.numeric(x) &&
      is_contactmatrix(x) &&
      length(unique(dim(x))) == 1L &&
      is.logical(attr(x, "symmetric"))
  )

}

#' Convert an object to a `contactmatrix` object.
#'
#' @param x Object to convert
#' @param ... Arguments passed to methods

#' @export
as_contactmatrix <- function(x, ...) {

  UseMethod("as_contactmatrix")

}

#' @rdname as_contactmatrix
#' @export
as_contactmatrix.default <- function(x, ...) {

  stop(
    "`as_contactmatrix()` method not defined for this class of object",
    call. = FALSE
  )

}

#' Convert a `matrix` to a `contactmatrix` object.
#'
#' The `matrix` has to already follow the `contactmatrix` convention of having
#' the survey participants as the rows.
#' This method will only add the correct class and attributes, but not touch
#' the data itself.
#'
#' @param x Array to convert
#' @inheritParams new_contactmatrix
#' @inheritParams as_contactmatrix
#'
#' @export
as_contactmatrix.matrix <- function(x, symmetric = NA, ...) {
  # TODO: see what's missing to make this an array method

  class(x) <- c("contactmatrix", class(x))
  attr(x, "symmetric") <- symmetric
  # It doesn't matter if we take rownames() or colnames() since they should be
  # identical
  attr(x, "groupings") <- colnames(x)

  if (!test_contactmatrix(x)) {
    stop(
      "Object cannot be converted to a valid `contactmatrix` object.",
      call. = FALSE
    )
  }

  return(x)

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
#' as defined in this package. You can use [assert_contactmatrix()] for this.
#'
#' @seealso [assert_contactmatrix()], [test_contactmatrix()]
#'
#' @export
is_contactmatrix <- function(x) {

  inherits(x, "contactmatrix")

}
