#' Check if a vector is NULL.
#'
#' This function checks if the provided vector is identical to NULL. This was created
#' no way to, in base R, check if an entire vector is NULL. We would usually need to
#' use any(is.null(vec)). I differed the syntax from base R's is.null by using an underscore
#' instead of a . (is_null insteade of is.null)
#'
#' @param vec The vector to check.
#' @return TRUE if the vector is NULL, FALSE otherwise.
#' @keywords internal
#' @export
is_null <- function(vec) {
  identical(vec, NULL)
}


