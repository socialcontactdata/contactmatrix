#' Get groupings of a `contactmatrix` or `contactmatrix_list` object
#'
#' @param x A `contactmatrix` or `contactmatrix_list` object to get groupings
#' from
#' @param ... Ignored for now
#'
#' @returns
#' - In the case of single groupings, a character vector of the groupings
#' - In the case of multiple groupings, a list of character vectors of the
#' groupings
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
#'   from  = c("[5,10)",  "[0,5)",   "[5,10)"),
#'   to    = c("[15,20)", "[10,15)", "[10,15)"),
#'   value = c(0.27   , 0.09   , 0.32   )
#' )
#'
#' cml <- new_contactmatrix_list(cm1, cm2)
#'
#' cm_get_groupings(cm1)
#' cm_get_groupings(cml)
#'
#' Multi-groupings
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
#' cm_get_groupings(cm3d)
cm_get_groupings <- function(x, ...) {
  UseMethod("cm_get_groupings")
}
