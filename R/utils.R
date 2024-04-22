# Simplify a list to a vector is it has just one (potentially vector) element
# and no names
simplify_list <- function(x) {

  if (is.list(x) && length(x) == 1L && is.null(names(x))) {
    return(x[[1L]])
  }

  return(x)

}
