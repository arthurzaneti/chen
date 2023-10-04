#' Check if a vector is NULL.
#'
#' This function checks if the provided vector is identical to NULL. This was created
#' since I found  no way to, in base R, check if an entire vector is NULL. We would usually need to
#' use \code{any(is.null(vec))}. I differed the syntax from base R's is.null by using an underscore
#' instead of a . (\code{is_null} instead of \code{is.null})
#'
#' @param vec The vector to check.
#' @return TRUE if the vector is NULL, FALSE otherwise.
#' @keywords internal
#' @examples
#' # No need for examples in internal function
#'
#' @export
is_null <- function(vec) {
  identical(vec, NULL)
}



#' @title Extend vector
#'
#' @description Extends a vecotr by adding zeros to the end until the desired length is reached, the desired
#' length needs to be more than the original length.
#'
#' @param vec The original vector
#' @param n The desired length
#'
#' @return Extended vector
#' @export
#'
#' @examples
extend_vec <- function(vec, n){
  new_vec <- numeric(n)
  new_vec[1:length(vec)] <- vec
  return(new_vec)
}
