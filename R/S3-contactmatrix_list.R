#' Aggregate the elements of a `contactmatrix_list`
#'
#' Aggregate the elements of a `contactmatrix_list` into a single
#' `contact_matrix`
#'
#' @param x A `contactmatrix_list` to aggregate
#' @param by Ignored for now
#' @param FUN Function to use to aggregate the elements of `x` into the output.
#'   Defaults to `mean`.
#' @param ... Addtional arguments passed to `FUN`
#'
#' @returns A `contact_matrix` with the same groupings and same groups (in
#' practice, same `dimnames()`) as all elements of the original
#' `contactmatrix_list` `x`.
#'
#' @export
aggregate.contactmatrix_list <- function(x, by, FUN = mean, ...) {

  if (!missing(by)) {
    stop(
      "`by` argument is not implemented yet. ",
      "It is only possible to summarize a contactmatrix_list into a single ",
      "contactmatrix.",
      call. = FALSE
    )
  }

  res <- apply(simplify2array(x), seq_along(dimnames(x[[1]])), FUN, ...)

  class(res) <- c("contact_matrix", class(res))
  # in the general case, this kind of operation won't result in a perfectly
  # symmetric matrix
  attr(res, "symmetric") <- FALSE

  return(res)
}
