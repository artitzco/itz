#' Lines Function
#'
#' Description:
#'   This function creates a new global parameter for controlling line properties in a plot.
#'
#' @param name (character) The name of the parameter.
#' @param ... Additional arguments to be passed.
#'
#' @return (NULL)
#'
#' @export
#'
#' @examples
#' Lines("mylines", alp.lines = 0.5, col.lines = "blue", lty.lines = 2, lwd.lines = 2)
#'
#' @references
#'   For more information, refer to the documentation of the `newGlobalParam` function.
#'
#' @seealso
#'   \code{\link{newGlobalParam}}
#'
#' @keywords line plot global parameter
Lines <- function(name, ...) {
  newGlobalParam(
    "x0000000mbf0N4sBjYI2uWKd29PXxpWMoUgANOR27kPg4h4JPEJ",
    function(add) {
      add("alp.lines", 1)
      add("col.lines", NULL)
      add("lty.lines", 1)
      add("lwd.lines", 1)
    }
  )(name, ...)
}

#' Points Function
#'
#' Description:
#'   This function creates a new global parameter for controlling point properties in a plot.
#'
#' @param name (character) The name of the parameter.
#' @param ... Additional arguments to be passed.
#'
#' @return (NULL)
#'
#' @export
#'
#' @examples
#' Points("mypoints", alp.points = 0.5, col.points = "red", lwd.points = 2)
#'
#' @references
#'   For more information, refer to the documentation of the `newGlobalParam` function.
#'
#' @seealso
#'   \code{\link{newGlobalParam}}
#'
#' @keywords point plot global parameter
Points <- function(name, ...) {
  newGlobalParam(
    "x0000000XOj4zAq3BxojQ4xB5kdXREfiFpVerO9qR6XWoYkvC9r",
    function(add) {
      add("alp.points", 1)
      add("col.points", NULL)
      add("lwd.points", 1)
    }
  )(name, ...)
}

#' Abline Function
#'
#' Description:
#'   This function creates a new global parameter for controlling line properties in a plot.
#'
#' @param name (character) The name of the parameter.
#' @param ... Additional arguments to be passed.
#'
#' @return (NULL)
#'
#' @export
#'
#' @examples
#' Abline("myline", alp.abline = 0.5, col.abline = "blue", lwd.abline = 2, sld.abline = FALSE)
#'
#' @references
#'   For more information, refer to the documentation of the `newGlobalParam` function.
#'
#' @seealso
#'   \code{\link{newGlobalParam}}
#'
#' @keywords line plot global parameter
Abline <- function(name, ...) {
  newGlobalParam(
    "x00000000wB8kBq0tLHhQYm1uVm3uQ5F7QN5nkC45U0ebFG8uhc",
    function(add) {
      add("alp.abline", 1)
      add("col.abline", NULL)
      add("lty.abline", 1)
      add("lwd.abline", 1)
      add("sld.abline", TRUE)
    }
  )(name, ...)
}

#' Plot Function
#'
#' Description:
#'   This function creates a new global parameter for controlling plot properties.
#'
#' @param name (character) The name of the parameter.
#' @param ... Additional arguments to be passed.
#'
#' @return (NULL)
#'
#' @export
#'
#' @examples
#' Plot("myplot", alp.backlines = 0.5, alp.background = 1, alp.border = 0, cex.axis = 1.1,
#'      cex.lab = 1.3, cex.main = 1.3, col.background = "#E0E0E0", col.backlines = "white",
#'      col.border = "black", labX = NA, labY = NA, lty.backlines = 1, lwd.backlines = 1,
#'      lx = NA, ly = NA, plot.lines = TRUE, plot.points = FALSE, px = 6, py = 6,
#'      xdigits = NA, ydigits = NA, xaxis = TRUE, yaxis = TRUE, xaxis.pos = TRUE, yaxis.pos = TRUE)
#'
#' @references
#'   For more information, refer to the documentation of the `newGlobalParam` function.
#'
#' @seealso
#'   \code{\link{newGlobalParam}}
#'
#' @keywords plot global parameter
Plot <- function(name, ...) {
  newGlobalParam(
    "x00000s84o5c8Px2ImUj0GxXsQzq0rsUrq5oL2deyXPrw1lPxGw",
    function(add) {
      add("alp.backlines", 0.5)
      add("alp.background", 1)
      add("alp.border", 0)
      add("cex.axis", 1.1)
      add("cex.lab", 1.3)
      add("cex.main", 1.3)
      add("col.background", "#E0E0E0")
      add("col.backlines", "white")
      add("col.border", "black")
      add("labX", NA)
      add("labY", NA)
      add("lty.backlines", 1)
      add("lwd.backlines", 1)
      add("lx", NA)
      add("ly", NA)
      add("plot.lines", TRUE)
      add("plot.points", FALSE)
      add("px", 6)
      add("py", 6)
      add("xdigits", NA)
      add("ydigits", NA)
      add("xaxis", T)
      add("yaxis", T)
      add("xaxis.pos", T)
      add("yaxis.pos", T)
    }
  )(name, ...)
}

#' Bar Function
#'
#' Description:
#'   This function creates a new global parameter for controlling bar plot properties.
#'
#' @param name (character) The name of the parameter.
#' @param ... Additional arguments to be passed.
#'
#' @return (NULL)
#'
#' @export
#'
#' @examples
#' Bar("mybar", alp.bar = 1, col.bar = "#A6A6A6", lwd.bar = 2, lty.bar = 1, horizontal = TRUE)
#'
#' @references
#'   For more information, refer to the documentation of the `newGlobalParam` function.
#'
#' @seealso
#'   \code{\link{newGlobalParam}}
#'
#' @keywords bar global parameter
Bar <- function(name, ...) {
  newGlobalParam(
    "x000000bPQwasoSlZDX0Ka3Ef89h0d8s89g90hVqdwGw37B1tGu",
    function(add) {
      add("alp.bar", 1)
      add("col.bar", "#A6A6A6")
      add("lwd.bar", 2)
      add("lty.bar", 1)
      add("horizontal", T)
    }
  )(name, ...)
}

#' Hist Function
#'
#' Description:
#'   This function creates a new global parameter for controlling histogram plot properties.
#'
#' @param name (character) The name of the parameter.
#' @param ... Additional arguments to be passed.
#'
#' @return (NULL)
#'
#' @export
#'
#' @examples
#' Hist("myhist", relative = TRUE, horizontal = TRUE, new.plot = TRUE, alp.hist = 0.7,
#'      col.hist = NULL, lty.hist = 1, lwd.hist = 2)
#'
#' @references
#'   For more information, refer to the documentation of the `newGlobalParam` function.
#'
#' @seealso
#'   \code{\link{newGlobalParam}}
#'
#' @keywords histogram global parameter
Hist <- function(name, ...) {
  newGlobalParam(
    "x00000000mF3zti9sP0msATjc7UEkz5BNIAQ4rr2fVGsy3NHzBX",
    function(add) {
      add("relative", T)
      add("horizontal", T)
      add("new.plot", T)
      add("alp.hist", 0.7)
      add("col.hist", NULL)
      add("lty.hist", 1)
      add("lwd.hist", 2)
    }
  )(name, ...)
}

#' Shadow Function
#'
#' Description:
#'   This function creates a new global parameter for controlling shadow plot properties.
#'
#' @param name (character) The name of the parameter.
#' @param ... Additional arguments to be passed.
#'
#' @return (NULL)
#'
#' @export
#'
#' @examples
#' Shadow("myshadow", alp.border = 1, alp.shadow = 0.5, alp.mean = 1,
#'        col.border = NULL, col.shadow = NULL, col.mean = NULL,
#'        lty.border = 1, lty.mean = 1, lwd.border = 0, lwd.mean = 1,
#'        plot.shadow = TRUE, plot.mean = TRUE)
#'
#' @references
#'   For more information, refer to the documentation of the `newGlobalParam` function.
#'
#' @seealso
#'   \code{\link{newGlobalParam}}
#'
#' @keywords shadow global parameter
Shadow <- function(name, ...) {
  newGlobalParam(
    "x0000000s95B0TU1PJbJ10FAcYgxrv7sbuv6yHd9d8g8DFf0Md4",
    function(add) {
      add("alp.border", 1)
      add("alp.shadow", 0.5)
      add("alp.mean", 1)
      add("col.border", NULL)
      add("col.shadow", NULL)
      add("col.mean", NULL)
      add("lty.border", 1)
      add("lty.mean", 1)
      add("lwd.border", 0)
      add("lwd.mean", 1)
      add("plot.shadow", T)
      add("plot.mean", T)
    }
  )(name, ...)
}

#' Legend Function
#'
#' Description:
#'   This function creates a new global parameter for controlling legend properties.
#'
#' @param name (character) The name of the parameter.
#' @param ... Additional arguments to be passed.
#'
#' @return (NULL)
#'
#' @export
#'
#' @examples
#' Legend("mylegend", cex.legend = 1.3, col.legend = "#000000", alp.legend = 1,
#'        lin = TRUE, col.lin = NULL, alp.lin = 1, lwd.lin = 1, lty.lin = 1,
#'        col.box = "#BEBEBE", alp.box = 1, lwd.box = 1, lty.box = 1,
#'        col.back = "#E6E6E6", alp.back = 0.8)
#'
#' @references
#'   For more information, refer to the documentation of the `newGlobalParam` function.
#'
#' @seealso
#'   \code{\link{newGlobalParam}}
#'
#' @keywords legend global parameter
Legend <- function(name, ...) {
  newGlobalParam(
    "x0000000wTaxROM6a5g511AuPnDtqxelpLNHv4GYcw43tdsAder",
    function(add) {
      add("cex.legend", 1.3)
      add("col.legend", "#000000")
      add("alp.legend", 1)
      add("lin", T)
      add("col.lin", NULL)
      add("alp.lin", 1)
      add("lwd.lin", 1)
      add("lty.lin", 1)
      add("col.box", "#BEBEBE")
      add("alp.box", 1)
      add("lwd.box", 1)
      add("lty.box", 1)
      add("col.back", "#E6E6E6")
      add("alp.back", 0.8)
    }
  )(name, ...)
}

#' mlines: Multiple Lines Plotting Function
#'
#' This function allows you to create a multiple lines plot by specifying the
#' x and y coordinates. It supports customization of line colors, line
#' transparency, line types, and line widths.
#'
#' @param x A vector or list of x-coordinates.
#' @param y A vector or list of y-coordinates.
#' @param col.lines The color(s) of the lines. Default is NA.
#' @param alp.lines The transparency level of the lines. Default is NA.
#' @param lty.lines The line type(s) of the lines. Default is NA.
#' @param lwd.lines The line width(s) of the lines. Default is NA.
#' @param ... Additional parameters.
#'
#' @examples
#' x <- 1:10
#' y1 <- x
#' y2 <- 2*x
#' y3 <- 3*x
#' mlines(x, y1, col.lines = "red", alp.lines = 0.5)
#' mlines(x, y2, col.lines = "blue", lty.lines = 2)
#' mlines(x, y3, col.lines = "green", lwd.lines = 2)
#'
#' @references
#' For more information, see the documentation of the `lines` function in the base package.
#'
#' @seealso
#' \code{\link{lines}}
#'
#' @keywords plotting lines multiple
#'
#' @export
#'
mlines <- function(x = NULL, y = NULL,
                   col.lines = nasd(),
                   alp.lines = nasd(),
                   lty.lines = nasd(),
                   lwd.lines = nasd(),
                   ...) {
  lst <- list(...)
  param <- lst$param
  lst$param <- NULL
  mlst <- c(lst, param, Lines("\\get.list"))
  if (is.nasd(alp.lines)) alp.lines <- mlst$alp.lines
  if (is.nasd(col.lines)) col.lines <- mlst$col.lines
  if (is.nasd(lty.lines)) lty.lines <- mlst$lty.lines
  if (is.nasd(lwd.lines)) lwd.lines <- mlst$lwd.lines
  col.lines <- fixCol(isnt.null(col.lines, mlst$col.lines.auxiliar))

  if (base::is.null(y) & !base::is.null(x)) {
    y <- x
    if (is.list(y)) {
      x <- list()
      for (i in 1:length(y)) x[[i]] <- 1:length(y[[i]])
    } else {
      x <- 1:length(y)
    }
  } else if (base::is.null(x) & base::is.null(y)) {
    x <- 0
    y <- 0
  }
  MLINES <- function(x, y, alp.lines, col.lines, lty.lines, lwd.lines) {
    if (lwd.lines >= 1) {
      sq <- seq(0, 1, 1.0 / lwd.lines)[-1]
      for (i in 1:lwd.lines) {
        lines(x, y, col = col.lines$getCol(bri = sq[i], alp = alp.lines, this = T), lty = lty.lines, lwd = 2 * (lwd.lines - i) + 1)
      }
      NO <- col.lines$getCol()
    }
  }

  if (!is.list(x) & !is.list(y)) {
    MLINES(x, y, alp.lines, col.lines, lty.lines, lwd.lines)
  } else {
    if (is.list(x) & is.list(y)) {
      if (length(x) == 1) {
        for (i in 1:length(y)) {
          MLINES(x[[1]], y[[i]], alp.lines, col.lines, lty.lines, lwd.lines)
        }
      } else if (length(y) == 1) {
        for (i in 1:length(x)) {
          MLINES(x[[i]], y[[1]], alp.lines, col.lines, lty.lines, lwd.lines)
        }
      } else {
        for (i in 1:length(y)) {
          MLINES(x[[i]], y[[i]], alp.lines, col.lines, lty.lines, lwd.lines)
        }
      }
    } else if (!is.list(x) & is.list(y)) {
      for (i in 1:length(y)) {
        MLINES(x, y[[i]], alp.lines, col.lines, lty.lines, lwd.lines)
      }
    } else if (is.list(x) & !is.list(y)) {
      for (i in 1:length(x)) {
        MLINES(x[[i]], y, alp.lines, col.lines, lty.lines, lwd.lines)
      }
    }
  }
}

#' mpoints: Multiple Points Plotting Function
#'
#' This function allows you to create a multiple points plot by specifying the
#' x and y coordinates. It supports customization of point colors, point
#' transparency, and point sizes.
#'
#' @param x A vector or list of x-coordinates.
#' @param y A vector or list of y-coordinates.
#' @param alp.points The transparency level of the points. Default is NA.
#' @param col.points The color(s) of the points. Default is NA.
#' @param lwd.points The size(s) of the points. Default is NA.
#' @param ... Additional parameters.
#'
#' @examples
#' x <- 1:10
#' y1 <- x
#' y2 <- 2*x
#' y3 <- 3*x
#' mpoints(x, y1, col.points = "red", alp.points = 0.5)
#' mpoints(x, y2, col.points = "blue", lwd.points = 2)
#' mpoints(x, y3, col.points = "green", lwd.points = 2)
#'
#' @references
#' For more information, see the documentation of the `points` function in the base package.
#'
#' @seealso
#' \code{\link{points}}
#'
#' @keywords plotting points multiple
#'
#' @export
#'
mpoints <- function(x = NULL, y = NULL,
                    alp.points = nasd(),
                    col.points = nasd(),
                    lwd.points = nasd(),
                    ...) {
  lst <- list(...)
  param <- lst$param
  lst$param <- NULL
  mlst <- c(lst, param, Points("\\get.list"))
  if (is.nasd(alp.points)) alp.points <- mlst$alp.points
  if (is.nasd(col.points)) col.points <- mlst$col.points
  if (is.nasd(lwd.points)) lwd.points <- mlst$lwd.points
  col.points <- fixCol(isnt.null(col.points, mlst$col.points.auxiliar))
  if (base::is.null(y) & !base::is.null(x)) {
    y <- x
    if (is.list(y)) {
      x <- list()
      for (i in 1:length(y)) x[[i]] <- 1:length(y[[i]])
    } else {
      x <- 1:length(y)
    }
  } else if (base::is.null(x) & base::is.null(y)) {
    x <- 0
    y <- 0
  }
  MPOINTS <- function(x, y, alp.points, col.points, lwd.points) {
    if (lwd.points >= 1) {
      sq <- seq(0, 1, 1.0 / lwd.points)[-1]
      for (i in 1:lwd.points) {
        points(x, y, col = col.points$getCol(bri = sq[i], alp = alp.points, this = T), lwd = 2 * (lwd.points - i) + 1)
      }
      NO <- col.points$getCol()
    }
  }

  if (!is.list(x) & !is.list(y)) {
    MPOINTS(x, y, alp.points, col.points, lwd.points)
  } else {
    if (is.list(x) & is.list(y)) {
      if (length(x) == 1) {
        for (i in 1:length(y)) {
          MPOINTS(x[[1]], y[[i]], alp.points, col.points, lwd.points)
        }
      } else if (length(y) == 1) {
        for (i in 1:length(x)) {
          MPOINTS(x[[i]], y[[1]], alp.points, col.points, lwd.points)
        }
      } else {
        for (i in 1:length(y)) {
          MPOINTS(x[[i]], y[[i]], alp.points, col.points, lwd.points)
        }
      }
    } else if (!is.list(x) & is.list(y)) {
      for (i in 1:length(y)) {
        MPOINTS(x, y[[i]], alp.points, col.points, lwd.points)
      }
    } else if (is.list(x) & !is.list(y)) {
      for (i in 1:length(x)) {
        MPOINTS(x[[i]], y, alp.points, col.points, lwd.points)
      }
    }
  }
}

#' mabline: Multiple Abline Plotting Function
#'
#' This function allows you to create multiple horizontal or vertical lines on a plot
#' using the `abline` function. It supports customization of line colors, line transparency,
#' line types, line widths, and whether the lines should be solid or dashed.
#'
#' @param h A vector of horizontal line positions.
#' @param v A vector of vertical line positions.
#' @param col.abline The color(s) of the lines. Default is NA.
#' @param alp.abline The transparency level of the lines. Default is NA.
#' @param lty.abline The line type(s) of the lines. Default is NA.
#' @param lwd.abline The line width(s) of the lines. Default is NA.
#' @param sld.abline Logical value indicating whether the lines should be solid
#'   or dashed. Default is NA.
#' @param ... Additional parameters.
#'
#' @examples
#' x <- 1:10
#' y <- x^2
#' plot(x, y)
#' mabline(h = 50, col.abline = "red", lwd.abline = 2)
#' mabline(v = 5:7, col.abline = "blue", lty.abline = 2)
#'
#' @references
#' For more information, see the documentation of the `abline` function in the base package.
#'
#' @seealso
#' \code{\link{abline}}
#'
#' @keywords plotting lines multiple
#'
#' @export
#'
mabline <- function(h = NULL, v = NULL,
                    col.abline = nasd(),
                    alp.abline = nasd(),
                    lty.abline = nasd(),
                    lwd.abline = nasd(),
                    sld.abline = nasd(),
                    ...) {
  lst <- list(...)
  param <- lst$param
  lst$param <- NULL
  mlst <- c(lst, param, Abline("\\get.list"))
  if (is.nasd(alp.abline)) alp.abline <- mlst$alp.abline
  if (is.nasd(col.abline)) col.abline <- mlst$col.abline
  if (is.nasd(lty.abline)) lty.abline <- mlst$lty.abline
  if (is.nasd(lwd.abline)) lwd.abline <- mlst$lwd.abline
  if (is.nasd(sld.abline)) sld.abline <- mlst$sld.abline
  col.abline <- fixCol(isnt.null(col.abline, mlst$col.abline.auxiliar))

  solida <- mlst$solida
  if (is.null(solida)) solida <- F
  sq <- if (lwd.abline != 1) seq(0, 1, 1.0 / lwd.abline)[-1] else 1
  mab <- function(fn, x) {
    for (j in 1:length(x)) {
      if (solida) {
        fn(x[j], lwd = lwd.abline, col = col.abline$getCol(alp = alp.abline, this = T), lty = lty.abline)
      } else if (lwd.abline >= 1) {
        for (i in 1:lwd.abline) {
          fn(x[j], lwd = if (sld.abline) 1 + lwd.abline - i else 2 * (lwd.abline - i) + 1, col = col.abline$getCol(bri = sq[i], alp = alp.abline, this = T), lty = lty.abline)
        }
        NO <- col.abline$getCol()
      }
    }
  }
  if (!is.null(h)) mab(function(x, ...) abline(h = x, ...), h)
  if (!is.null(v)) mab(function(x, ...) abline(v = x, ...), v)
}

pol <- function(xlim, ylim, px, py, lx = NA, ly = NA, labX = NA, labY = NA, cex = 1, background = color(0.086, 0.086, 0.086), col.backlines = color(.5, .5, .5), lwd = 2, lty = 1, alp = 1, xdigits = NA, ydigits = NA, alp.background = alp.background, xaxis = T, yaxis = T, xaxis.pos = T, yaxis.pos = T, col.border = "black", alp.border = 0, solida = T) {
  mx <- (xlim[2] - xlim[1]) / 2
  my <- (ylim[2] - ylim[1]) / 2
  x <- c(xlim[1] - mx, xlim[2] + mx)
  y <- c(ylim[1] - my, ylim[2] + my)

  fitDig <- function(lim, n, digits) {
    sq <- seq(lim[1], lim[2], (lim[2] - lim[1]) / (n - 1))
    if (is.na(digits)) {
      return(sq)
    }
    sqRef <- round(sq, digits)
    its.ok <- function(x) {
      for (i in 2:length(x)) {
        if (x[i] == x[i - 1]) {
          return(FALSE)
        }
      }
      return(TRUE)
    }
    while (!its.ok(sqRef)) {
      digits <- digits + 1
      sqRef <- round(sq, digits)
    }
    sqRef
  }
  if (is.na(lx[1])) lx <- fitDig(xlim, px, xdigits)
  if (is.na(ly[1])) ly <- fitDig(ylim, py, ydigits)
  if (is.na(labX[1])) labX <- T
  if (is.na(labY[1])) labY <- T
  rect(par("usr")[1], par("usr")[3],
    par("usr")[2], par("usr")[4],
    col = background$getCol(alp = alp.background),
    border = NA
  )
  mabline(v = lx, h = ly, col.abline = col.backlines, lwd.abline = lwd, lty.abline = lty, alp.abline = alp, solida = solida)
  if (xaxis) axis(if (xaxis.pos) 1 else 3, at = lx, labels = labX, cex.axis = cex)
  if (yaxis) axis(if (yaxis.pos) 2 else 4, at = ly, labels = labY, cex.axis = cex)
  rect(par("usr")[1], par("usr")[3],
    par("usr")[2], par("usr")[4],
    col = NA,
    border = col.border$getCol(alp = alp.border)
  )
  globalVar("xlim.0000000k1DTt5P2hOzSqOZFzG04ip6a3xfdFA9DrDp8Ffilq", xlim)
  globalVar("ylim.0000000k1DTt5P2hOzSqOZFzG04ip6a3xfdFA9DrDp8Ffilq", ylim)
}

#' mplot: Multiple Plot Function
#'
#' This function allows you to create multiple plots with customizable settings
#' such as axis labels, plot title, background color, line and point properties,
#' and more. It supports plotting multiple sets of x-y data, specifying the x and
#' y limits, and customizing various visual elements of the plot.
#'
#' @param x A list of x values for each data set. If a single vector is provided,
#'   it will be treated as a single data set.
#' @param y A list of y values for each data set. If a single vector is provided,
#'   it will be treated as a single data set.
#' @param xlim The x-axis limits of the plot. Default is NULL.
#' @param ylim The y-axis limits of the plot. Default is NULL.
#' @param col The color(s) to use for the plot. Default is NULL.
#' @param main The main title of the plot. Default is NA.
#' @param xlab The label for the x-axis. Default is NA.
#' @param ylab The label for the y-axis. Default is NA.
#' @param alp.background The transparency level of the plot background. Default is NA.
#' @param alp.backlines The transparency level of the background grid lines. Default is NA.
#' @param alp.border The transparency level of the plot border. Default is NA.
#' @param cex.axis The font size for the axis labels. Default is NA.
#' @param cex.lab The font size for the axis labels. Default is NA.
#' @param cex.main The font size for the plot title. Default is NA.
#' @param col.background The color of the plot background. Default is NA.
#' @param col.backlines The color of the background grid lines. Default is NA.
#' @param col.border The color of the plot border. Default is NA.
#' @param labX The label for the x-axis tick marks. Default is NA.
#' @param labY The label for the y-axis tick marks. Default is NA.
#' @param lty.backlines The line type of the background grid lines. Default is NA.
#' @param lwd.backlines The line width of the background grid lines. Default is NA.
#' @param lx The position of the x-axis tick marks. Default is NA.
#' @param ly The position of the y-axis tick marks. Default is NA.
#' @param plot.lines Logical value indicating whether to plot lines. Default is NA.
#' @param plot.points Logical value indicating whether to plot points. Default is NA.
#' @param px The number of x-axis tick marks. Default is NA.
#' @param py The number of y-axis tick marks. Default is NA.
#' @param xdigits The number of digits to display on the x-axis tick marks. Default is NA.
#' @param ydigits The number of digits to display on the y-axis tick marks. Default is NA.
#' @param xaxis Logical value indicating whether to display the x-axis. Default is NA.
#' @param yaxis Logical value indicating whether to display the y-axis. Default is NA.
#' @param xaxis.pos Logical value indicating whether to display the x-axis tick marks.
#'   Default is NA.
#' @param yaxis.pos Logical value indicating whether to display the y-axis tick marks.
#'   Default is NA.
#' @param ... Additional parameters.
#'
#' @examples
#' x <- 1:10
#' y1 <- x^2
#' y2 <- 2 * x
#' mplot(x = list(x, x), y = list(y1, y2), main = "Multiple Plots", xlab = "X", ylab = "Y")
#'
#' @references
#' For more information, see the documentation of the `plot` function in the base package.
#'
#' @seealso
#' \code{\link{plot}}
#'
#' @keywords plotting multiple plots
#'
#' @export
#'
mplot <- function(x = NULL,
                  y = NULL,
                  xlim = NULL,
                  ylim = NULL,
                  col = NULL,
                  main = NA,
                  xlab = NA,
                  ylab = NA,
                  alp.background = nasd(),
                  alp.backlines = nasd(),
                  alp.border = nasd(),
                  cex.axis = nasd(),
                  cex.lab = nasd(),
                  cex.main = nasd(),
                  col.background = nasd(),
                  col.backlines = nasd(),
                  col.border = nasd(),
                  labX = nasd(),
                  labY = nasd(),
                  lty.backlines = nasd(),
                  lwd.backlines = nasd(),
                  lx = nasd(),
                  ly = nasd(),
                  plot.lines = nasd(),
                  plot.points = nasd(),
                  px = nasd(),
                  py = nasd(),
                  xdigits = nasd(),
                  ydigits = nasd(),
                  xaxis = nasd(),
                  yaxis = nasd(),
                  xaxis.pos = nasd(),
                  yaxis.pos = nasd(),
                  ...) {
  lst <- list(...)
  param <- lst$param
  lst$param <- NULL
  mlst <- c(lst, param, Plot("\\get.list"))
  if (is.nasd(alp.backlines)) alp.backlines <- mlst$alp.backlines
  if (is.nasd(alp.background)) alp.background <- mlst$alp.background
  if (is.nasd(alp.border)) alp.border <- mlst$alp.border
  if (is.nasd(cex.axis)) cex.axis <- mlst$cex.axis
  if (is.nasd(cex.lab)) cex.lab <- mlst$cex.lab
  if (is.nasd(cex.main)) cex.main <- mlst$cex.main
  if (is.nasd(col.background)) col.background <- mlst$col.background
  if (is.nasd(col.backlines)) col.backlines <- mlst$col.backlines
  if (is.nasd(col.border)) col.border <- mlst$col.border
  if (is.nasd(labX)) labX <- mlst$labX
  if (is.nasd(labY)) labY <- mlst$labY
  if (is.nasd(lty.backlines)) lty.backlines <- mlst$lty.backlines
  if (is.nasd(lwd.backlines)) lwd.backlines <- mlst$lwd.backlines
  if (is.nasd(lx)) lx <- mlst$lx
  if (is.nasd(ly)) ly <- mlst$ly
  if (is.nasd(plot.lines)) plot.lines <- mlst$plot.lines
  if (is.nasd(plot.points)) plot.points <- mlst$plot.points
  if (is.nasd(px)) px <- mlst$px
  if (is.nasd(py)) py <- mlst$py
  if (is.nasd(xdigits)) xdigits <- mlst$xdigits
  if (is.nasd(ydigits)) ydigits <- mlst$ydigits
  if (is.nasd(xaxis)) xaxis <- mlst$xaxis
  if (is.nasd(yaxis)) yaxis <- mlst$yaxis
  if (is.nasd(xaxis.pos)) xaxis.pos <- mlst$xaxis.pos
  if (is.nasd(yaxis.pos)) yaxis.pos <- mlst$yaxis.pos
  col.background <- fixCol(isnt.null(col.background, mlst$col.background.auxiliar))
  col.backlines <- fixCol(isnt.null(col.backlines, mlst$col.backlines.auxiliar))
  col.border <- fixCol(isnt.null(col.border, mlst$col.border.auxiliar))
  xlab <- if (is.expression(xlab)) xlab else isnt.na(xlab, isnt.null(mlst$xlab, NA))
  ylab <- if (is.expression(ylab)) ylab else isnt.na(ylab, isnt.null(mlst$ylab, NA))
  main <- if (is.expression(main)) main else isnt.na(main, isnt.null(mlst$main, NA))
  if (base::is.null(y) & !base::is.null(x)) {
    y <- x
    if (is.list(y)) {
      x <- list()
      for (i in 1:length(y)) x[[i]] <- 1:length(y[[i]])
    } else {
      x <- 1:length(y)
    }
  } else if (base::is.null(x) & base::is.null(y)) {
    x <- 0
    y <- 0
  }
  IF(!is.list(x), x <- list(x))
  IF(!is.list(y), y <- list(y))
  if (is.null(xlim)) {
    xmin <- Inf
    xmax <- -Inf
    for (i in 1:length(x)) {
      xmin <- min(xmin, x[[i]], na.rm = T)
      xmax <- max(xmax, x[[i]], na.rm = T)
    }
    xlim <- IF(xmin == xmax, c(xmin - 1, xmin + 1), c(xmin, xmax))
  }
  if (is.null(ylim)) {
    ymin <- Inf
    ymax <- -Inf
    for (i in 1:length(y)) {
      ymin <- min(ymin, y[[i]], na.rm = T)
      ymax <- max(ymax, y[[i]], na.rm = T)
    }
    ylim <- IF(ymin == ymax, c(ymin - 1, ymin + 1), c(ymin, ymax))
  }

  plot(0, 0, xlim = xlim, ylim = ylim, axes = F, main = main, xlab = xlab, ylab = ylab, cex.main = cex.main, cex.lab = cex.lab, type = "n")
  pol(xlim, ylim, px, py, lx, ly, labX, labY, cex.axis, col.background, col.backlines, lwd.backlines, lty.backlines, alp.backlines, xdigits, ydigits, alp.background, xaxis, yaxis, xaxis.pos, yaxis.pos, col.border, alp.border, solida = isnt.null(mlst$solida, T))
  col <- fixCol(col)
  if (plot.lines) {
    mlst.lines <- Lines("\\get.list")
    alp.lines <- isnt.null(mlst$alp.lines, mlst.lines$alp.lines)
    lwd.lines <- isnt.null(mlst$lwd.lines, mlst.lines$lwd.lines)
    lty.lines <- isnt.null(mlst$lty.lines, mlst.lines$lty.lines)
    col.lines <- isnt.null(mlst$col.lines, mlst.lines$col.lines)
    mlines(x, y, alp.lines = alp.lines, col.lines.auxiliar = col, col.lines = col.lines, lty.lines = lty.lines, lwd.lines = lwd.lines)
  }

  if (plot.points) {
    mlst.points <- Points("\\get.list")
    alp.points <- isnt.null(mlst$alp.points, mlst.points$alp.points)
    lwd.points <- isnt.null(mlst$lwd.points, mlst.points$lwd.points)
    col.points <- isnt.null(mlst$col.points, mlst.points$col.points)
    mpoints(x, y, alp.points = alp.points, col.points.auxiliar = col, col.points = col.points, lwd.points = lwd.points)
  }
}

#' mbar: Multiple Bar Plot Function
#'
#' This function allows you to create multiple bar plots with customizable settings
#' such as bar color, transparency, line width, line type, and orientation. It supports
#' plotting multiple sets of x-y data as bars and allows for horizontal or vertical orientation.
#'
#' @param x A numeric vector representing the x-axis values.
#' @param y A numeric vector representing the height of the bars.
#' @param col.bar The color of the bars. Default is NULL.
#' @param alp.bar The transparency level of the bars. Default is NA.
#' @param lwd.bar The line width of the bars. Default is NA.
#' @param lty.bar The line type of the bars. Default is NA.
#' @param horizontal Logical value indicating whether to plot horizontal bars.
#'   Default is NA.
#' @param ... Additional parameters.
#'
#' @examples
#' x <- c("A", "B", "C")
#' y <- c(10, 15, 12)
#' mbar(x, y, col.bar = "blue", alp.bar = 0.5, lwd.bar = 2)
#'
#' @seealso
#' \code{\link{barplot}}
#'
#' @keywords plotting multiple barplot
#'
#' @export
#'
mbar <- function(x, y, ##### Añadir plot
                 col.bar = nasd(),
                 alp.bar = nasd(),
                 lwd.bar = nasd(),
                 lty.bar = nasd(),
                 horizontal = nasd(),
                 ...) {
  lst <- list(...)
  param <- lst$param
  lst$param <- NULL
  mlst <- c(lst, param, Bar("\\get.list"))
  if (is.nasd(alp.bar)) alp.bar <- mlst$alp.bar
  if (is.nasd(col.bar)) col.bar <- mlst$col.bar
  if (is.nasd(lwd.bar)) lwd.bar <- mlst$lwd.bar
  if (is.nasd(lty.bar)) lty.bar <- mlst$lty.bar
  if (is.nasd(horizontal)) horizontal <- mlst$horizontal
  col.bar <- fixCol(isnt.null(col.bar, mlst$col.bar.auxiliar))
  od <- order(x)
  x <- x[od]
  y <- y[od]
  w <- abs(diff(x) / 2)
  w <- c(w[1], w, w[length(w)])
  x <- x - w[1:length(x)]
  w <- w + c(w[2:length(w)], 0)
  x[length(w)] <- x[length(x)] + w[length(x)]
  for (i in 1:length(y)) {
    X <- c(x[i], x[i], x[i + 1], x[i + 1])
    Y <- c(0, y[i], y[i], 0)
    if (horizontal) {
      polygon(X, Y, col = col.bar$getCol(alp = alp.bar), lwd = lwd.bar, lty = lty.bar)
    } else {
      polygon(Y, X, col = col.bar$getCol(alp = alp.bar), lwd = lwd.bar, lty = lty.bar)
    }
  }
}

#' mhist: Multiple Histogram Plot Function
#'
#' This function allows you to create multiple histogram plots with customizable settings
#' such as relative frequencies, orientation (horizontal or vertical), new plot creation,
#' histogram transparency, histogram color, line type, line width, and additional parameters.
#' It supports plotting histograms for a single variable.
#'
#' @param x A numeric vector representing the data values for which the histogram will be created.
#' @param relative Logical value indicating whether to plot relative frequencies instead of counts.
#'   Default is NA.
#' @param horizontal Logical value indicating whether to create a horizontal histogram.
#'   Default is NA.
#' @param new.plot Logical value indicating whether to create a new plot. If TRUE, a new plot will be created,
#'   and any existing plot settings (e.g., xlim, ylim) will be ignored. Default is NA.
#' @param alp.hist The transparency level of the histogram. Default is NA.
#' @param col.hist The color of the histogram. Default is NA.
#' @param lty.hist The line type of the histogram. Default is NA.
#' @param lwd.hist The line width of the histogram. Default is NA.
#' @param ... Additional parameters.
#'
#' @examples
#' x <- rnorm(100)
#' mhist(x, col.hist = "skyblue", alp.hist = 0.7, lwd.hist = 2)
#'
#' @seealso
#' \code{\link{hist}}, \code{\link{mbar}}, \code{\link{mplot}}
#'
#' @keywords plotting multiple histogramplot
#'
#' @export
#'
mhist <- function(x,
                  relative = nasd(),
                  horizontal = nasd(),
                  new.plot = nasd(),
                  alp.hist = nasd(),
                  col.hist = nasd(),
                  lty.hist = nasd(),
                  lwd.hist = nasd(),
                  ...) {
  lst <- list(...)
  param <- lst$param
  lst$param <- NULL
  mlst <- c(lst, param, Hist("\\get.list"))
  if (is.nasd(relative)) relative <- mlst$relative
  if (is.nasd(horizontal)) horizontal <- mlst$horizontal
  if (is.nasd(new.plot)) new.plot <- mlst$new.plot
  if (is.nasd(alp.hist)) alp.hist <- mlst$alp.hist
  if (is.nasd(col.hist)) col.hist <- mlst$col.hist
  if (is.nasd(lty.hist)) lty.hist <- mlst$lty.hist
  if (is.nasd(lwd.hist)) lwd.hist <- mlst$lwd.hist
  col.hist <- fixCol(isnt.null(col.hist, mlst$col.hist.auxiliar))
  ht <- hist(x, plot = F, breaks = isnt.null(mlst[["breaks"]], "Sturges"))
  x <- ht$mids
  y <- if (relative) ht$density else ht$counts
  if (new.plot) {
    lst <- list(...)
    xlim <- isnt.null(
      lst$xlim,
      isnt.null(
        Plot("xlim"),
        IF(horizontal, c(min(x) - (x[2] - x[1]) / 2, max(x) + (x[2] - x[1]) / 2), c(0, max(y)))
      )
    )
    ylim <- isnt.null(
      lst$ylim,
      isnt.null(
        Plot("ylim"),
        IF(horizontal, c(0, max(y)), c(min(x) - (x[2] - x[1]) / 2, max(x) + (x[2] - x[1]) / 2))
      )
    )
    mplot(param = mlst, plot.lines = F, plot.points = F, xlim = xlim, ylim = ylim)
  }
  mbar(x, y, horizontal = horizontal, col.bar = col.hist, alp.bar = alp.hist, lwd.bar = lwd.hist, lty.bar = lty.hist)
  if (isnt.null(mlst[["ret"]], F)) {
    return(list(breaks = ht$breaks, counts = ht$counts, mids = ht$mids))
  }
}

#' mshadow: Multiple Shadow Plot Function
#'
#' This function allows you to create multiple shadow plots with customizable settings
#' such as border color, transparency, line type, line width, shadow transparency,
#' shadow color, mean transparency, mean color, and plot options. It supports plotting
#' shadows and mean lines for multiple sets of x-y data.
#'
#' @param x1 A numeric vector representing the x-axis values for the lower bound of the shadow.
#' @param y1 A numeric vector representing the y-axis values for the lower bound of the shadow.
#' @param x2 A numeric vector representing the x-axis values for the upper bound of the shadow.
#'   Default is NULL, in which case x1 will be used as the upper bound.
#' @param y2 A numeric vector representing the y-axis values for the upper bound of the shadow.
#'   Default is NULL, in which case y1 will be used as the upper bound.
#' @param alp.border The transparency level of the shadow border. Default is NA.
#' @param col.border The color of the shadow border. Default is NA.
#' @param lty.border The line type of the shadow border. Default is NA.
#' @param lwd.border The line width of the shadow border. Default is NA.
#' @param alp.shadow The transparency level of the shadow. Default is NA.
#' @param col.shadow The color of the shadow. Default is NA.
#' @param alp.mean The transparency level of the mean line. Default is NA.
#' @param col.mean The color of the mean line. Default is NA.
#' @param lwd.mean The line width of the mean line. Default is NA.
#' @param lty.mean The line type of the mean line. Default is NA.
#' @param plot.mean Logical value indicating whether to plot the mean line. Default is NA.
#' @param plot.shadow Logical value indicating whether to plot the shadow. Default is NA.
#' @param ... Additional parameters.
#'
#' @examples
#' x1 <- c(1, 2, 3)
#' y1 <- c(10, 15, 12)
#' x2 <- c(1, 2, 3)
#' y2 <- c(8, 12, 10)
#' mshadow(x1, y1, x2, y2, col.shadow = "gray", alp.shadow = 0.5, col.mean = "red", alp.mean = 0.8)
#'
#' @seealso
#' \code{\link{polygon}}, \code{\link{mlines}}
#'
#' @keywords plotting multiple shadowplot
#'
#' @export
#'
mshadow <- function(x1, y1 = NULL, x2 = NULL, y2 = NULL,
                    alp.border = nasd(),
                    col.border = nasd(),
                    lty.border = nasd(),
                    lwd.border = nasd(),
                    alp.shadow = nasd(),
                    col.shadow = nasd(),
                    alp.mean = nasd(),
                    col.mean = nasd(),
                    lwd.mean = nasd(),
                    lty.mean = nasd(),
                    plot.mean = nasd(),
                    plot.shadow = nasd(),
                    ...) {
  lst <- list(...)
  param <- lst$param
  lst$param <- NULL
  mlst <- c(lst, param, Shadow("\\get.list"))
  if (is.nasd(alp.border)) alp.border <- mlst$alp.border
  if (is.nasd(alp.shadow)) alp.shadow <- mlst$alp.shadow
  if (is.nasd(alp.mean)) alp.mean <- mlst$alp.mean
  if (is.nasd(col.border)) col.border <- mlst$col.border
  if (is.nasd(col.shadow)) col.shadow <- mlst$col.shadow
  if (is.nasd(col.mean)) col.mean <- mlst$col.mean
  if (is.nasd(lty.border)) lty.border <- mlst$lty.border
  if (is.nasd(lty.mean)) lty.mean <- mlst$lty.mean
  if (is.nasd(lwd.border)) lwd.border <- mlst$lwd.border
  if (is.nasd(lwd.mean)) lwd.mean <- mlst$lwd.mean
  if (is.nasd(plot.shadow)) plot.shadow <- mlst$plot.shadow
  if (is.nasd(plot.mean)) plot.mean <- mlst$plot.mean
  col.border <- fixCol(isnt.null(col.border, mlst$col.border.auxiliar))
  col.shadow <- fixCol(isnt.null(col.shadow, mlst$col.shadow.auxiliar))
  col.mean <- fixCol(isnt.null(col.mean, mlst$col.mean.auxiliar))
  table.exist <- FALSE
  if (base::is.null(x2)) {
    table <- x1
    x1 <- table$z
    x2 <- x1
    y1 <- table$min
    y2 <- table$max
    table.exist <- TRUE
  }
  if (is.null(col.mean)) col.mean <- color()
  if (is.null(col.border)) col.border <- color()
  if (is.null(col.shadow)) col.shadow <- color()
  x <- c(x1, x2[length(x2):1], x1[1])
  y <- c(y1, y2[length(y2):1], y1[1])
  if (plot.shadow) {
    polygon(x, y, col = col.shadow$getCol(alp.shadow), border = "#00000000")
    mlines(x, y, lty.lines = lty.border, lwd.lines = lwd.border, col.lines = col.border, alp.lines = alp.border)
  }
  if (plot.mean & table.exist) {
    mlines(x1, table$mean, lty.lines = lty.mean, lwd.lines = lwd.mean, col.lines = col.mean, alp.lines = alp.mean)
  }
}

posLegend <- function(pos, inf) {
  x <- inf$rect$left
  y <- inf$rect$top
  usr <- par("usr")
  X <- abs(c(usr[1], usr[2]) - globalVar("xlim.0000000k1DTt5P2hOzSqOZFzG04ip6a3xfdFA9DrDp8Ffilq"))
  Y <- abs(c(usr[3], usr[4]) - globalVar("ylim.0000000k1DTt5P2hOzSqOZFzG04ip6a3xfdFA9DrDp8Ffilq"))
  if (pos == "bottomleft" | pos == "bottom" | pos == "bottomright") y <- y + Y[1]
  if (pos == "topleft" | pos == "top" | pos == "topright") y <- y - Y[2]
  if (pos == "bottomleft" | pos == "left" | pos == "topleft") x <- x + X[1]
  if (pos == "bottomright" | pos == "right" | pos == "topright") x <- x - X[2]
  list(x = x, y = y)
}

#' mlegend: Multiple Legend Plot Function
#'
#' This function allows you to create multiple legends in a plot with customizable settings
#' such as legend position, text size, text color, line type, line width, box color, box transparency,
#' background color, and additional parameters.
#'
#' @param pos The position of the legend. Can be a character string indicating a predefined position
#'   ("topleft", "topright", "bottomleft", "bottomright", "left", "right", "bottom", "top", "center"),
#'   or a numeric vector specifying the x and y coordinates of the legend. Default is "center".
#' @param legend The text labels for the legend.
#' @param cex.legend The size of the legend text. Default is NA.
#' @param col.legend The color of the legend text. Default is NA.
#' @param alp.legend The transparency level of the legend text. Default is NA.
#' @param lin The line types for the legend lines. Default is NA.
#' @param col.lin The colors of the legend lines. Default is NA.
#' @param alp.lin The transparency levels of the legend lines. Default is NA.
#' @param lwd.lin The line widths of the legend lines. Default is NA.
#' @param lty.lin The line types of the legend lines. Default is NA.
#' @param col.box The color of the legend box. Default is NA.
#' @param alp.box The transparency level of the legend box. Default is NA.
#' @param lwd.box The line width of the legend box. Default is NA.
#' @param lty.box The line type of the legend box. Default is NA.
#' @param col.back The background color of the legend. Default is NA.
#' @param alp.back The transparency level of the legend background. Default is NA.
#' @param ... Additional parameters.
#'
#' @examples
#' mlegend(pos = "topright", legend = c("Group 1", "Group 2", "Group 3"),
#'         col.legend = c("red", "blue", "green"), lwd.lin = 2, col.box = "gray")
#'
#' @seealso
#' \code{\link{legend}}, \code{\link{posLegend}}
#'
#' @keywords plotting multiple legendplot
#'
#' @export
#'
mlegend <- function(pos = "center",
                    legend = "",
                    cex.legend = nasd(),
                    col.legend = nasd(),
                    alp.legend = nasd(),
                    lin = nasd(),
                    col.lin = nasd(),
                    alp.lin = nasd(),
                    lwd.lin = nasd(),
                    lty.lin = nasd(),
                    col.box = nasd(),
                    alp.box = nasd(),
                    lwd.box = nasd(),
                    lty.box = nasd(),
                    col.back = nasd(),
                    alp.back = nasd(),
                    ...) {
  lst <- list(...)
  param <- lst$param
  lst$param <- NULL
  mlst <- c(lst, param, Legend("\\get.list"))
  if (is.nasd(cex.legend)) cex.legend <- mlst$cex.legend
  if (is.nasd(col.legend)) col.legend <- mlst$col.legend
  if (is.nasd(alp.legend)) alp.legend <- mlst$alp.legend
  if (is.nasd(lin)) lin <- mlst$lin
  if (is.nasd(col.lin)) col.lin <- mlst$col.lin
  if (is.nasd(alp.lin)) alp.lin <- mlst$alp.lin
  if (is.nasd(lwd.lin)) lwd.lin <- mlst$lwd.lin
  if (is.nasd(lty.lin)) lty.lin <- mlst$lty.lin
  if (is.nasd(col.box)) col.box <- mlst$col.box
  if (is.nasd(alp.box)) alp.box <- mlst$alp.box
  if (is.nasd(lwd.box)) lwd.box <- mlst$lwd.box
  if (is.nasd(lty.box)) lty.box <- mlst$lty.box
  if (is.nasd(col.back)) col.back <- mlst$col.back
  if (is.nasd(alp.back)) alp.back <- mlst$alp.back
  col.legend <- fixCol(isnt.null(col.legend, mlst$col.legend.auxiliar))
  col.lin <- fixCol(isnt.null(col.lin, mlst$col.lin.auxiliar))
  col.box <- fixCol(isnt.null(col.box, mlst$col.box.auxiliar))
  col.back <- fixCol(isnt.null(col.back, mlst$col.back.auxiliar))
  if (is.character(pos)) {
    inf <- graphics::legend(pos, legend = legend, lty = lty.lin, lwd = lwd.lin, cex = cex.legend, box.lty = lty.box, box.lwd = lwd.box, pch = NA, plot = FALSE)
    pl <- posLegend(pos, inf)
    x <- pl$x
    y <- pl$y
  } else {
    x <- pos[1]
    y <- pos[2]
  }
  colNA <- "#00000000"
  graphics::legend(x, y, legend = legend, lty = lty.lin, lwd = lwd.lin, cex = cex.legend, box.lty = lty.box, bg = col.back$getCol(alp = alp.back), box.lwd = lwd.box, text.col = col.legend$getCol(alp = alp.legend), box.col = colNA, col = colNA, pch = NA)
  n <- max(lwd.box, lwd.lin)
  if (n > 0) {
    lwd <- lwd.box
    lwd.box <- rep(0, n)
    if (lwd > 0) lwd.box[1:lwd] <- 1 + lwd - (1:lwd)
    col <- col.box
    col.box <- rep(colNA, n)
    if (lwd > 0) {
      sq <- seq(0, 1, 1.0 / lwd)[-1]
      for (i in 1:lwd) {
        col.box[i] <- col$getCol(bri = sq[i], alp = alp.box, this = T)
      }
      NO <- col$getCol()
    }

    lin.iter <- iterVector(lin, cyclic = T)
    lwd.iter <- iterVector(lwd.lin, cyclic = T)
    col.iter <- iterCol(col.lin, cyclic = T)
    lty.lin.iter <- iterVector(lty.lin, cyclic = T)
    alp.lin.iter <- iterVector(alp.lin, cyclic = T)
    lwd.lin.mat <- NULL
    col.lin.mat <- NULL
    pch <- NULL
    lty.lin <- NULL
    alp.lin <- NULL
    for (i in 1:length(legend)) {
      pch[i] <- if (lin.iter$Next()) NA else 1
      lty.lin[i] <- lty.lin.iter$Next()
      alp.lin[i] <- alp.lin.iter$Next()
      if (!is.na(pch[i])) lty.lin[i] <- 0
      lwd <- lwd.iter$Next()
      lwd.lin <- rep(0, n)
      if (lwd > 0) lwd.lin[1:lwd] <- 2 * (lwd - (1:lwd)) + 1
      col <- col.iter$Next()
      col.lin <- rep(colNA, n)
      if (lwd > 0) {
        sq <- seq(0, 1, 1.0 / lwd)[-1]
        for (j in 1:lwd) {
          col.lin[j] <- col$getCol(bri = sq[j], alp = alp.lin[i], this = T)
        }
        NO <- col$getCol()
      }
      lwd.lin.mat <- cbind(lwd.lin.mat, lwd.lin)
      col.lin.mat <- cbind(col.lin.mat, col.lin)
    }
    for (i in 1:n) {
      graphics::legend(x, y,
        legend = legend, lty = lty.lin, cex = cex.legend,
        box.lty = lty.box, bg = colNA, text.col = colNA,
        lwd = lwd.lin.mat[i, ], col = col.lin.mat[i, ],
        box.lwd = lwd.box[i], box.col = col.box[i], pch = pch
      )
    }
  }
}

#' subgraphic: Subplot Function for Multiple Plots
#'
#' This function allows you to create subplots by arranging multiple plots together in a single figure.
#'
#' @param plots The plots to be displayed as subplots.
#' @param xmar The margins for the x-axis in the subplots. If not specified, the default margins will be used.
#' @param ymar The margins for the y-axis in the subplots. If not specified, the default margins will be used.
#' @param xfig The figure dimensions for the x-axis in the subplots. If not specified, the default dimensions will be used.
#' @param yfig The figure dimensions for the y-axis in the subplots. If not specified, the default dimensions will be used.
#' @param overlap A logical value indicating whether the subplots should overlap or be displayed side by side.
#'   If set to TRUE, the subplots will overlap. If set to FALSE, the subplots will be displayed side by side.
#'
#' @examples
#' # Create two plots and display them as subplots
#' plot1 <- plot(rnorm(100))
#' plot2 <- plot(runif(100))
#' subgraphic(plots = list(plot1, plot2))
#'
#' @keywords plotting subplot subfigure multiple
#'
#' @export
#'
subgraphic <- function(plots,
                       xmar = NULL, ymar = NULL,
                       xfig = NULL, yfig = NULL,
                       overlap = T) {
  mar <- par("mar")
  fig <- par("fig")
  xmar <- if (!is.null(xmar[1])) xmar else mar[c(2, 4)]
  ymar <- if (!is.null(ymar[1])) ymar else mar[c(1, 3)]
  xfig <- if (!is.null(xfig[1])) xfig else fig[c(1, 2)]
  yfig <- if (!is.null(yfig[1])) yfig else fig[c(3, 4)]

  newMar <- c(ymar[1], xmar[1], ymar[2], xmar[2])
  newFig <- c(xfig, yfig)
  par(mar = newMar, fig = newFig, new = overlap)
  plots
  par(mar = mar, fig = fig)
}

#' substrae.LIST.DAT: Subtraction of Elements from List and Creation of New Data Object
#'
#' This function subtracts elements from a list based on a given condition and creates a new data object by transforming the selected elements.
#'
#' @param ... The elements to be processed as a list.
#' @param CONDITION The condition to determine which elements to subtract from the list.
#'   It should be a logical expression that evaluates to TRUE or FALSE for each element in the list.
#'   Elements that satisfy the condition will be removed from the list.
#' @param TRANSFORM The transformation function to be applied to the selected elements.
#'   It should be a function that takes an element as input and returns a transformed version of the element.
#'   The transformed elements will be stored in a new data object.
#'
#' @return A list containing two elements:
#'   - "lista.123": The updated list after subtracting the selected elements.
#'   - "data.123": The new data object containing the transformed elements.
#'
#' @examples
#' # Create a list of numbers
#' numbers <- list(1, 2, 3, 4, 5)
#'
#' # Subtract even numbers from the list and create a new data object with the squared values
#' result <- substrae.LIST.DAT(numbers, CONDITION = function(x) x %% 2 == 0, TRANSFORM = function(x) x^2)
#' result$lista.123  # Updated list after subtracting even numbers
#' result$data.123   # New data object with squared values
#'
#' @keywords list data subtraction transformation
#'
#' @export
#'
substrae.LIST.DAT <- function(..., CONDITION, TRANSFORM) {
  lst <- list(...)
  dat <- newList()
  ind <- NULL
  for (i in 1:length(lst)) {
    if (CONDITION(lst[[i]])) {
      dat$add(TRANSFORM(lst[[i]]))
      ind <- c(ind, i)
    }
  }
  lst[ind] <- NULL
  return(list(lista.123 = lst, data.123 = dat))
}

#' linesGroup: Grouped Lines Plot
#'
#' This function plots grouped lines based on the input data. It allows for customizing the appearance of the lines, points, shadows, and means within each group.
#'
#' @param ... The elements to be processed as a list.
#' @param plot.lines Logical vector indicating whether to plot lines for each group.
#'   If a single logical value is provided, it will be recycled for each group.
#' @param plot.points Logical vector indicating whether to plot points for each group.
#'   If a single logical value is provided, it will be recycled for each group.
#' @param plot.shadow Logical vector indicating whether to plot shadows for each group.
#'   If a single logical value is provided, it will be recycled for each group.
#' @param plot.mean Logical vector indicating whether to plot means for each group.
#'   If a single logical value is provided, it will be recycled for each group.
#' @param col Vector of colors for the groups. If not provided, default colors will be used.
#'
#' @examples
#' # Create data for three groups
#' group1 <- data.frame(x = 1:10, y = 1:10)
#' group2 <- data.frame(x = 1:10, y = seq(1, 10, by = 2))
#' group3 <- data.frame(x = 1:10, y = seq(1, 10, by = 3))
#'
#' # Plot grouped lines with shadows and means
#' linesGroup(group1, group2, group3, plot.lines = TRUE, plot.shadow = TRUE, plot.mean = TRUE)
#'
#' @export
#'
linesGroup <- function(..., plot.lines = F, plot.points = F, plot.shadow = F, plot.mean = F, col = NULL) {
  lst <- list(...)
  if (is.null(lst$lista.123)) {
    ret <- substrae.LIST.DAT(...,
      CONDITION = is.group.class,
      TRANSFORM = function(x) x
    )
    lst <- ret$lista.123
    dat <- ret$data.123
  } else {
    dat <- lst$data.123
    lst <- lst$lista.123
  }
  ## Shadow
  plot.shadow <- iterVector(plot.shadow, cyclic = T)
  iter <- dat$iter()
  iter.col <- iterCol(col, cyclic = T, defVal = nasd())
  iter.col.border <- iterCol(lst$col.border, cyclic = T, defVal = nasd())
  iter.alp.border <- iterVector(lst$alp.border, cyclic = T, defVal = nasd())
  iter.lwd.border <- iterVector(lst$lwd.border, cyclic = T, defVal = nasd())
  iter.lty.border <- iterVector(lst$lty.border, cyclic = T, defVal = nasd())
  iter.col.shadow <- iterCol(lst$col.shadow, cyclic = T, defVal = nasd())
  iter.alp.shadow <- iterVector(lst$alp.shadow, cyclic = T, defVal = nasd())
  while (iter$HasNext()) {
    grupo <- iter$Next()
    col <- iter.col$Next()
    col.border <- iter.col.border$Next()
    alp.border <- iter.alp.border$Next()
    lwd.border <- iter.lwd.border$Next()
    lty.border <- iter.lty.border$Next()
    col.shadow <- iter.col.shadow$Next()
    alp.shadow <- iter.alp.shadow$Next()
    if (plot.shadow$Next()) {
      mshadow(grupo$table,
        alp.border = alp.border, lwd.border = lwd.border,
        lty.border = lty.border, col.border = col.border,
        alp.shadow = alp.shadow, col.shadow = col.shadow,
        col.border.auxiliar = isnt.nasd(col, grupo$col$clone()),
        col.shadow.auxiliar = isnt.nasd(col, grupo$col$clone()),
        plot.shadow = T, plot.mean = F
      )
    }
  }
  ## lines
  plot.lines <- iterVector(plot.lines, cyclic = T)
  iter$Restart()
  iter.col$Restart()
  iter.col.lines <- iterCol(lst$col.lines, cyclic = T, defVal = nasd())
  iter.alp.lines <- iterVector(lst$alp.lines, cyclic = T, defVal = nasd())
  iter.lwd.lines <- iterVector(lst$lwd.lines, cyclic = T, defVal = nasd())
  iter.lty.lines <- iterVector(lst$lty.lines, cyclic = T, defVal = nasd())
  while (iter$HasNext()) {
    grupo <- iter$Next()
    col <- iter.col$Next()
    col.lines <- iter.col.lines$Next()
    alp.lines <- iter.alp.lines$Next()
    lwd.lines <- iter.lwd.lines$Next()
    lty.lines <- iter.lty.lines$Next()
    if (plot.lines$Next()) {
      mlines(grupo$X, grupo$Y,
        alp.lines = alp.lines, lwd.lines = lwd.lines,
        lty.lines = lty.lines, col.lines = col.lines,
        col.lines.auxiliar = isnt.nasd(col, grupo$col$clone())
      )
    }
  }
  ## points
  plot.points <- iterVector(plot.points, cyclic = T)
  iter$Restart()
  iter.col$Restart()
  iter.col.points <- iterCol(lst$col.points, cyclic = T, defVal = nasd())
  iter.alp.points <- iterVector(lst$alp.points, cyclic = T, defVal = nasd())
  iter.lwd.points <- iterVector(lst$lwd.points, cyclic = T, defVal = nasd())
  while (iter$HasNext()) {
    grupo <- iter$Next()
    col <- iter.col$Next()
    col.points <- iter.col.points$Next()
    alp.points <- iter.alp.points$Next()
    lwd.points <- iter.lwd.points$Next()
    if (plot.points$Next()) {
      mpoints(grupo$X, grupo$Y,
        alp.points = alp.points,
        lwd.points = lwd.points, col.points = col.points,
        col.points.auxiliar = isnt.nasd(col, grupo$col$clone())
      )
    }
  }
  ## mean
  plot.mean <- iterVector(plot.mean, cyclic = T)
  iter$Restart()
  iter.col$Restart()
  iter.col.mean <- iterCol(lst$col.mean, cyclic = T, defVal = nasd())
  iter.alp.mean <- iterVector(lst$alp.mean, cyclic = T, defVal = nasd())
  iter.lwd.mean <- iterVector(lst$lwd.mean, cyclic = T, defVal = nasd())
  iter.lty.mean <- iterVector(lst$lty.mean, cyclic = T, defVal = nasd())
  while (iter$HasNext()) {
    grupo <- iter$Next()
    col <- iter.col$Next()
    col.mean <- iter.col.mean$Next()
    alp.mean <- iter.alp.mean$Next()
    lwd.mean <- iter.lwd.mean$Next()
    lty.mean <- iter.lty.mean$Next()
    if (plot.mean$Next()) {
      mshadow(grupo$table,
        alp.mean = alp.mean, lwd.mean = lwd.mean,
        lty.mean = lty.mean, col.mean = col.mean,
        col.mean.auxiliar = isnt.nasd(col, grupo$col$clone()),
        plot.mean = T, plot.shadow = F
      )
    }
  }
}

#' plotGroup: Grouped Plot
#'
#' This function creates a grouped plot based on the input data. It allows for customizing the appearance of the lines, points, shadows, and means within each group.
#'
#' @param ... The elements to be processed as a list.
#' @param new.plot Logical value indicating whether to create a new plot or overlay on an existing plot.
#' @param plot.lines Logical vector indicating whether to plot lines for each group.
#'   If a single logical value is provided, it will be recycled for each group.
#' @param plot.points Logical vector indicating whether to plot points for each group.
#'   If a single logical value is provided, it will be recycled for each group.
#' @param plot.shadow Logical vector indicating whether to plot shadows for each group.
#'   If a single logical value is provided, it will be recycled for each group.
#' @param plot.mean Logical vector indicating whether to plot means for each group.
#'   If a single logical value is provided, it will be recycled for each group.
#' @param col Vector of colors for the groups. If not provided, default colors will be used.
#'
#' @examples
#' # Create data for three groups
#' group1 <- data.frame(x = 1:10, y = 1:10)
#' group2 <- data.frame(x = 1:10, y = seq(1, 10, by = 2))
#' group3 <- data.frame(x = 1:10, y = seq(1, 10, by = 3))
#'
#' # Create a new plot with grouped lines
#' plotGroup(group1, group2, group3, plot.lines = TRUE)
#'
#' # Overlay grouped lines on an existing plot
#' plot(x = 1:10, y = 1:10)
#' plotGroup(group1, group2, group3, new.plot = FALSE, plot.lines = TRUE)
#'
#' @export
#'
plotGroup <- function(..., new.plot = T, plot.lines = F, plot.points = F, plot.shadow = F, plot.mean = F, col = NULL) {
  if (!new.plot) {
    linesGroup(...,
      plot.lines = plot.lines, plot.points = plot.points,
      plot.shadow = plot.shadow, plot.mean = plot.mean, col = col
    )
  } else {
    lst <- list(...)
    if (is.null(lst$lista.123)) {
      ret <- substrae.LIST.DAT(...,
        CONDITION = is.group.class,
        TRANSFORM = function(x) x
      )
      lst <- ret$lista.123
      dat <- ret$data.123
    } else {
      dat <- lst$data.123
      lst <- lst$lista.123
    }
    if (base::is.null(lst$xlim) | base::is.null(lst$ylim)) {
      xmin <- Inf
      xmax <- -Inf
      ymin <- Inf
      ymax <- -Inf
      iter <- dat$iter()
      while (iter$HasNext()) {
        grupo <- iter$Next()
        x <- grupo$X
        y <- grupo$Y
        for (i in 1:length(x)) {
          xmin <- min(xmin, x[[i]], na.rm = T)
          xmax <- max(xmax, x[[i]], na.rm = T)
          ymin <- min(ymin, y[[i]], na.rm = T)
          ymax <- max(ymax, y[[i]], na.rm = T)
        }
      }
    }
    xlim <- isnt.null(
      lst$xlim,
      IF(xmin == xmax, c(xmin - 1, xmin + 1), c(xmin, xmax))
    )
    ylim <- isnt.null(
      lst$ylim,
      IF(ymin == ymax, c(ymin - 1, ymin + 1), c(ymin, ymax))
    )
    mplot(xlim = xlim, ylim = ylim, plot.lines = F, plot.points = F, param = lst)
    linesGroup(
      lista.123 = lst, data.123 = dat,
      plot.lines = plot.lines, plot.points = plot.points, plot.shadow = plot.shadow, plot.mean = plot.mean, col = col
    )
  }
}

#' printer: PDF Printer
#'
#' This function creates a PDF printer that allows you to easily generate PDF files with custom settings.
#'
#' @param dir Directory where the PDF files will be saved. If not provided, the files will be saved in the current working directory.
#' @param prop Proportion of the width to the height of the PDF page.
#' @param width Width of the PDF page in inches.
#' @param height Height of the PDF page in inches.
#' @param size Vector specifying the number of rows and columns for arranging multiple plots on a single page.
#'   The first element represents the number of rows, and the second element represents the number of columns.
#'
#' @return A list of functions that can be used to control the PDF printer.
#'
#' @details
#' The PDF printer provides the following functions:
#'   - `PDF`: Generates a PDF file with the specified settings and plots.
#'   - `ON`: Activates the PDF printer, allowing plots to be saved as PDF files.
#'   - `OFF`: Deactivates the PDF printer, allowing plots to be displayed on the screen instead.
#'
#' @examples
#' # Create a PDF printer with default settings
#' myprinter <- printer()
#'
#' # Activate the PDF printer
#' myprinter$ON()
#'
#' # Generate a PDF file with a plot
#' myprinter$PDF(plot(1:10))
#'
#' # Deactivate the PDF printer
#' myprinter$OFF()
#'
#' @export
#'
printer <- function(dir = "", prop = 0.68, width = 9, height = NULL, size = c(1, 1)) {
  if (dir != "") {
    create.dir(dir)
    dir <- pst(dir, "/")
  }
  ACTIVE <- newVec(T)
  PROP <- newVec(prop)
  WIDTH <- newVec(width)
  HEIGTH <- newVec(if (is.numeric(height)) height else NA)
  SIZE <- newVec(size)

  PDF <- function(file = NULL, prop = NULL, width = NULL, height = NULL, size = NULL, PLOTS = NULL) {
    if (ACTIVE$get()) {
      FIN()
      if (base::is.null(file)) {
        i <- 1
        while (file.exists(pst(dir, "myplot_", i, ".pdf"))) {
          i <- i + 1
        }
        file <- pst(dir, "myplot_", i, ".pdf")
      } else {
        file <- pst(dir, file)
      }
      prop <- isnt.null(prop, PROP$get())
      width <- isnt.null(width, WIDTH$get())
      height <- isnt.null(height, isnt.na(HEIGTH$get(), prop * width))
      size <- isnt.null(size, SIZE$get())
      pdf(file = file, width = width * size[2], height = height * size[1])
      PLOTS
      FIN()
    } else {
      PLOTS
    }
  }
  FIN <- function() {
    ret <- TRY(dev.off(), "")
    while (ret != "") {
      print(ret)
      ret <- TRY(dev.off(), "")
    }
  }
  ON <- function() ACTIVE$set(TRUE)
  OFF <- function() ACTIVE$set(FALSE)
  return(list(PDF = PDF, ON = ON, OFF = OFF))
}
