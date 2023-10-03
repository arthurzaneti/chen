#' Printing for CHARMA objects
#'
#' @param x CHARMA object
#' @param ... Further arguments to be passedo or from other methods
#' @export
print.CHARMA <- function(x, ...) {
  cat("Names: ", x$names,"\n")
  cat("Coefficients: ", format(round(x$coef, 3), nsmall = 3), "\n")
  cat("Case: ", x$case, "\n")
}

#' Printing for reg_chen objects
#'
#' @param x reg_chen object
#' @param ... Further arguments to be passedo or from other methods
#' @export
print.reg_chen <- function(x, ...) {
  cat("Names: ", x$names,"\n")
  cat("Coefficients: ", format(round(x$coef, 3), nsmall = 3), "\n")
}

#' Printing for reg_CHARMA objects
#'
#' @param x reg_CHARMA object
#' @param ... Further arguments to be passedo or from other methods
#' @export
print.reg_CHARMA <- function(x, ...) {
  cat("Names: ", x$names,"\n")
  cat("Coefficients: ", format(round(x$coef, 3), nsmall = 3), "\n")
  cat("Case: ", x$case, "\n")
}
