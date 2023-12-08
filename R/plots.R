#' @title Plots for reg_chen, CHARMA and reg_ARMA objects
#'
#' @param x The object
#' @param layout Could be \code{"all"} so all plots are plotted in the same image (2x2 grid)
#' or \code{"sep"} so plots are displayed separatelly.
#'
#' @return The plotted graphs
#' @importFrom car qqPlot
#' @export
#'
#' @examples
plot.reg_chen <- function(x, layout = "all"){
  checkmate::check_class(x, "chen_reg")
  checkmate::check_choice(layout, c("all", "sep"))
  #____________________________________________________________________________
  if(layout == "all"){
    opar <- par(no.readonly = T)
    par(mfrow = c(2, 2))
  }

  add_margin <- 0.05
  ylim <- c(min(c(-3 - add_margin, min(x$residuals) - add_margin)),
             max(c(3 + add_margin, max(x$residuals) + add_margin)))
  #____1____
  hist(x$residuals,
       main = "Quantile Residuals",
       xlab = "Values")
  graphics::box()

  #____2____
  plot(x$residuals,
       main = "Residuals vs Index",
       ylab = "Residuals", ylim = ylim)
  abline(2, 0, lty = 6)
  abline(-2, 0, lty = 6)
  abline(3, 0, lty = 2)
  abline(-3, 0, lty = 2)

  #____3____
  plot(x$residuals, x$fitted,
       xlab = "Residuals",
       ylab = "Fitted Values",
       main = "Residuals vs Fitted Values")
  #____4____
  car::qqPlot(x$residuals, col=c("black"), col.lines = "black",
              ylab = "Residuals", xlab = "Norm Quantiles", grid = F,
              main = "Confidence Envelope")

  if(layout == "all") par(opar)
}
#______________________________________________________________________________________________________
#'@describeIn plot.reg_chen CHARMA objects
#'@export
plot.CHARMA <- function(x, layout = "all"){
  checkmate::check_class(x, "CHARMA")
  checkmate::check_choice(layout, c("all", "sep"))
  #______________________________________________________________________________
  if(layout == "all"){
    opar <- par(no.readonly = T)
    par(mfrow = c(2, 2))
  }
  add_margin <- 0.05
  ylim <- c(min(c(-3 - add_margin, min(x$residuals) - add_margin)),
            max(c(3 + add_margin, max(x$residuals) + add_margin)))

  #____1____
  hist(x$residuals,
       main = "Quantile Residuals",
       xlab = "Values")
  graphics::box()

  #____2____
  plot(x$residuals,
       main = "Residuals vs Index",
       ylab = "Residuals", ylim = ylim)
  abline(2, 0, lty = 6)
  abline(-2, 0, lty = 6)
  abline(3, 0, lty = 2)
  abline(-3, 0, lty = 2)

  #____3____
  plot(x$residuals, x$fitted[3:length(x$fitted)],
       xlab = "Residuals",
       ylab = "Fitted Values",
       main = "Residuals vs Fitted Values")
  #____4____
  invisible(car::qqPlot(x$residuals, col=c("black"), col.lines = "black",
              ylab = "Residuals", xlab = "Norm Quantiles", grid = F,
              main = "Confidence Envelope"))
  if(layout == "all") par(opar)
}

#______________________________________________________________________________________________________
#'@describeIn plot.reg_chen reg_CHARMA objects
plot.reg_CHARMA <- function(x, layout = "all"){
  checkmate::check_class(x, "reg_CHARMA")
  checkmate::check_choice(layout, c("all", "sep"))
  #______________________________________________________________________________
  if(layout == "all"){
    opar <- par(no.readonly = T)
    par(mfrow = c(2, 2))
  }
  add_margin <- 0.05
  ylim <- c(min(c(-3 - add_margin, min(x$residuals) - add_margin)),
            max(c(3 + add_margin, max(x$residuals) + add_margin)))

  #____1____
  hist(x$residuals,
       main = "Quantile Residuals",
       xlab = "Values")
  graphics::box()

  #____2____
  plot(x$residuals,
       main = "Residuals vs Index",
       ylab = "Residuals", ylim = ylim)
  abline(2, 0, lty = 6)
  abline(-2, 0, lty = 6)
  abline(3, 0, lty = 2)
  abline(-3, 0, lty = 2)

  #____3____
  plot(x$residuals, x$fitted[3:length(x$fitted)],
       xlab = "Residuals",
       ylab = "Fitted Values",
       main = "Residuals vs Fitted Values")
  #____4____
  invisible(car::qqPlot(x$residuals, col=c("black"), col.lines = "black",
                        ylab = "Residuals", xlab = "Norm Quantiles", grid = F,
                        main = "Confidence Envelope"))
  if(layout == "all") par(opar)
}
