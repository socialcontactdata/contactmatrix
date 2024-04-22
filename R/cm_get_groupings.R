#' Get groupings of a `contactmatrix` or `contactmatrix_list` object
#'
#' @param x A `contactmatrix` or `contactmatrix_list` object to get groupings
#' from
#' @param ... Ignored for now
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
#'
#' cm_get_groupings(cm1)
#' cm_get_groupings(cml)
cm_get_groupings <- function(x, ...) {
  UseMethod("cm_get_groupings")
}
