#' Constructor for a `contactmatrix` object
#'
#' @param from
#' @param to
#' @param value
#' @param fill Numeric value to use for contacts non-defined in the `from`,
#'   `to`, `value` argument triplet. Defaults to `0`; i.e., no contact.
#'
#' @export
#'
#' @examples
#' library(tibble)
#'
#' new_contactmatrix(
#'   from = c("00-05","05-10", "05-10"),
#'   to = c("00-05", "10-15", "15-20"),
#'   value = c(0.32, 0.46, 0.72)
#' )
#'
#' dat <- tribble(
#'   ~age,  ~gender,    ~age,  ~gender, ~value,
#'   "young",   "male",   "old", "female",     1L,
#'   "young", "female",   "old", "female",     2L,
#'   "old", "female", "young", "female",     2L
#' )
#'
#' from <- .subset(dat, 1:2)
#' to <- .subset(dat, 3:4)
#' value <- .subset2(dat, 5L)
#'
#' new_contactmatrix(from, to, value)
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
