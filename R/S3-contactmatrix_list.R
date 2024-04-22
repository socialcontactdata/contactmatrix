#' Print a `contactmatrix_list`
#'
#' @param x A `contactmatrix_list` object
#' @param ... Ignored for now
#'
#' @returns `x`, invisibly
#'
#' @export
#'
#' @examples
#' cm1 <- new_contactmatrix(
#'   from  = c("[0,5)", "[5,10)",  "[5,10)"),
#'   to    = c("[0,5)", "[10,15)", "[15,20)"),
#'   value = c(0.32   , 0.46   , 0.72   )
#' )
#'
#' cm2 <- new_contactmatrix(
#'   from  = c("[5,10)", "[5,10)", "[5,10)"),
#'   to    = c("[0,5)",  "[10,15)", "[10,15)"),
#'   value = c(0.27   , 0.09   , 0.32   )
#' )
#'
#' cml <- new_contactmatrix_list(cm1, cm2)
#'
#' print(cml)
#'
print.contactmatrix_list <- function(x, ...) {
  cat("Contact matrix list with", length(x), "elements.\n")
  cat("Each element has the following structure:\n")
  e <- x[[1]]
  print(e)
  invisible(x)
}

#' Aggregate the elements of a `contactmatrix_list`
#'
#' Aggregate the elements of a `contactmatrix_list` into a single
#' `contact_matrix`
#'
#' @param x A `contactmatrix_list` to aggregate
#' @param by Ignored for now
#' @param FUN Function to use to aggregate the elements of `x` into the output.
#'   Defaults to `mean`.
#' @param ... Additional arguments passed to `FUN`
#'
#' @returns A `contact_matrix` with the same groupings and same groups (in
#' practice, same `dimnames()`) as all elements of the original
#' `contactmatrix_list` `x`.
#'
#' @importFrom stats aggregate
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

  # TODO: add input checks in FUN

  res <- apply(simplify2array(x), seq_along(dimnames(x[[1]])), FUN, ...)

  class(res) <- c("contact_matrix", class(res))
  # in the general case, this kind of operation won't result in a perfectly
  # symmetric matrix
  attr(res, "symmetric") <- FALSE

  return(res)
}

#' @aliases cm_get_groupings
#'
#' @export
cm_get_groupings.contactmatrix_list <- function(x, ...) {

  # It's safe to do this since by definition, contactmatrix_list elements all
  # have the same groupings
  return(attr(x[[1]], "groupings"))

}
