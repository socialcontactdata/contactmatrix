#' @aliases cm_get_groupings
#'
#' @export
cm_get_groupings.contactmatrix <- function(x, ...) {

  return(attr(x, "groupings"))

}

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
#' cm2d <- new_contactmatrix(
#'   from  = c("[0,5)", "[5,10)",  "[5,10)"),
#'   to    = c("[0,5)", "[10,15)", "[15,20)"),
#'   value = c(0.32   , 0.46   , 0.72   ),
#'   symmetric = TRUE
#' )
#' print(cm2d)
#'
#' # Multi-groupings
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
#' print(cm3d)
print.contactmatrix <- function(x, ...) {

  # Skip linting as cli usage is not covered:
  # https://github.com/r-lib/lintr/issues/2252
  # nolint next: object_usage_linter.
  cm_symmetry <- switch(
    as.character(attr(x, "symmetric")),
    "TRUE" = "Symmetric ",
    "FALSE" = "Asymmetric ",
    ""
  )

  cli::cli_h2("{cm_symmetry}Contact matrix")
  cli::cli_h3("Groupings")
  print(cm_get_groupings(x))

  cli::cli_h3("Contact rates")
  matx <- x
  class(matx) <- setdiff(class(matx), "contactmatrix")
  attr(matx, "groupings") <- NULL
  attr(matx, "symmetric") <- NULL
  print(matx)

  return(invisible(x))

}

#' @export
#'
#' @importFrom utils head
head.contactmatrix <- function(x, n, ...) {

  out <- NextMethod()
  class(out) <- class(x)
  # Should we clip the grouping to n? It depends on whether we expect head() to
  # be used purely for printing (we want to keep the original groupings) or for
  # transforming the object (we want to clip the groupings)
  attr(out, "groupings") <- attr(x, "groupings")
  attr(out, "symmetric") <- attr(x, "symmetric")

  return(out)

}

#' Convert a `contactmatrix` to a tidy `data.frame`
#'
#' @param x A `contactmatrix` object
#' @param ... Ignored for now
#'
#' @importFrom generics tidy
#' @importFrom stats reshape
#'
#' @export
#'
#' @examples
#' cm2d <- new_contactmatrix(
#'   from  = c("[0,5)", "[5,10)",  "[5,10)"),
#'   to    = c("[0,5)", "[10,15)", "[15,20)"),
#'   value = c(0.32   , 0.46   , 0.72   ),
#'   symmetric = TRUE
#' )
#' tidy(cm2d)
tidy.contactmatrix <- function(x, ...) {

  gpings <- cm_get_groupings(x)
  if (is.list(gpings)) {
    stop("Cannot tidy a multi-groupings contact matrix yet", call. = FALSE)
  }

  out <- reshape(
    as.data.frame(x),
    idvar = "age_from",
    ids = gpings,
    times = gpings,
    timevar = "age_to",
    varying = list(gpings),
    direction = "long"
  )
  rownames(out) <- NULL
  colnames(out)[2] <- "contact"
  out[, c(3, 1, 2)]

}
