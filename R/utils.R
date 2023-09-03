#' Check if a vector is NULL.
#'
#' This function checks if the provided vector is identical to NULL.
#'
#' @param vec The vector to check.
#' @return TRUE if the vector is NULL, FALSE otherwise.
#' @keywords internal
#' @export
is_null <- function(vec) {
  identical(vec, NULL)
}


