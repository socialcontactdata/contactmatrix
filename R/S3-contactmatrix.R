#' @aliases cm_get_groupings
#'
#' @export
cm_get_groupings.contactmatrix <- function(x, ...) {

  return(attr(x, "groupings"))

}
