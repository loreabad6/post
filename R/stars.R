#' stars methods for post_array objects
#'
#' @param x a post_array object
#' @param ... additional arguments passed onto the respective stars function
#' @name stars

#' @rdname stars
#' @export
`[.post_array` = function(x, ...) {
  out = NextMethod()
  restore_post_array(out, x)
}
