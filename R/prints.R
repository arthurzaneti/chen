#' Printing for CHARMA objects
#'
#' @param x CHARMA object
#' @param ... Further arguments to be passedo or from other methods
#' @importFrom checkmate assert_class
#' @export
#'
print.CHARMA <- function(x, ...) {
  checkmate::assert_class(x, "CHARMA")
  cat("Names: ", x$names,"\n")
  cat("Coefficients: ", format(round(x$coef, 3), nsmall = 3), "\n")
  cat("Lambda: ", x$lambda, "\n")
  cat("Tau: ", x$tau, "\n")
}

#' Printing for reg_chen objects
#'
#' @param x reg_chen object
#' @param ... Further arguments to be passedo or from other methods
#' @importFrom checkmate assert_class
#' @export
print.reg_chen <- function(x, ...) {
  checkmate::assert_class(x, "reg_chen")
  cat("Names: ", x$names,"\n")
  cat("Coefficients: ", format(round(x$coef, 3), nsmall = 3), "\n")
  cat("Lambda: ", x$lambda, "\n")
  cat("Tau: ", x$tau, "\n")
}

#' Printing for reg_CHARMA objects
#'
#' @param x reg_CHARMA object
#' @param ... Further arguments to be passedo or from other methods
#' @importFrom checkmate assert_class
#' @export
print.reg_CHARMA <- function(x, ...) {
  checkmate::assert_class(x, "reg_CHARMA")
  cat("Names: ", x$names,"\n")
  cat("Coefficients: ", format(round(x$coef, 3), nsmall = 3), "\n")
  cat("Lambda: ", x$lambda, "\n")
  cat("Tau: ", x$tau, "\n")
}
