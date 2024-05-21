#' Make a `contactmatrix` object symmetric
#'
#' Symmetricity for social contact matrices is defined as
#' $c_{ij}N_i == c_{ji}N_j$ where c is the contact value and N the group size.
#'
#' @param x A `contactmatrix` object to make symmetric
#' @param population A numeric vector of the population size of each group
#'
#' @return A symmetric `contactmatrix` object
#'
#' @export
cm_make_symmetric <- function(x, population) {

  # TODO: add some input checking
  if (length(dim(x)) != 2) {
    stop("Only 2D contact matrices are supported for now", call. = FALSE)
  }

  if (attr(x, "symmetric")) {
    return(x)
  }

  popx <- population * x
  res <- (popx + t(popx)) / 2 / population

  attr(res, "symmetric") <- TRUE
  return(res)

}
