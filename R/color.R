
#' Fix Color
#'
#' This function fixes the color argument by converting it into a valid color object.
#' If the color argument is NULL, it returns a default color. If the color argument is
#' a character, it converts it into a color object. If the color argument is already a
#' valid color object, it returns it as is.
#'
#' @param col The color argument to fix. It can be NULL, a character, or a color object.
#'
#' @return A valid color object.
#'
#' @export
fixCol <- function(col) {
  IF(is.null(col), color(), IF(is.character(col), color(col), col))
}

#' Color
#'
#' This function creates a color object based on the provided RGB values. The RGB values
#' can be specified as numeric vectors or character names of colors. If the RGB values are
#' not provided, random values are used by default.
#'
#' @param r Numeric vector or character name representing the red component of the color.
#'   If a character name is provided, it will be converted into an RGB value.
#' @param g Numeric vector or character name representing the green component of the color.
#'   If a character name is provided, it will be converted into an RGB value.
#' @param b Numeric vector or character name representing the blue component of the color.
#'   If a character name is provided, it will be converted into an RGB value.
#' @param maxValue The maximum value for the RGB components. Default is 1.
#'
#' @return A color object with various functions to manipulate and combine colors.
#'
#' @examples
#' # Create a color object with specific RGB values
#' col1 <- color(r = 0.2, g = 0.4, b = 0.6)
#' col1$getRGB() # Get the RGB values of the color
#' col1$getCol() # Get the color as an R color object
#'
#' # Create a color object from a character name
#' col2 <- color("red")
#' col2$getRGB() # Get the RGB values of the color
#' col2$getCol() # Get the color as an R color object
#'
#' # Combine two colors
#' combined_col <- col1$combine(col2, r = 0.7)
#' combined_col$getRGB() # Get the RGB values of the combined color
#' combined_col$getCol() # Get the combined color as an R color object
#'
#' @export
color <- function(r = NA, g = NA, b = NA, maxValue = 1) {
  if (is.character(r)) {
    tab <- NULL
    for (i in 1:length(r)) {
      tab <- rbind(tab, TRY(col2rgb(r[i])[1:3] / 255, c(1, 1, 1)))
    }
    r <- tab[, 1]
    g <- tab[, 2]
    b <- tab[, 3]
  }
  if (length(r) > 1 | length(g) > 1 | length(b) > 1) {
    return(colorList(r, g, b, maxValue))
  }
  if (is.na(r)) {
    r <- runif(1)
  } else {
    r <- r / maxValue
  }
  if (is.na(g)) {
    g <- runif(1)
  } else {
    g <- g / maxValue
  }
  if (is.na(b)) {
    b <- runif(1)
  } else {
    b <- b / maxValue
  }

  getRGB <- function(this = F) c(r, g, b)

  getCol <- function(alp = 1, bri = 1, this = F) {
    rgb(bri * r, bri * g, bri * b, alp)
  }

  getRGBs <- function() {
    cbind(r, g, b)
  }

  getCols <- function(alp = 1, bri = 1) {
    n <- max(length(alp), length(bri))
    alp <- iterVector(alp, cyclic = T)
    bri <- iterVector(bri, cyclic = T)
    color1 <- NULL
    for (i in 1:n) {
      color1[i] <- rgb(bri$This() * r, bri$This() * g, bri$Next() * b, alp$Next())
    }
    color1 <- rbind(color1)
    colnames(color1) <- paste("t", 1:n, sep = "")
    color1
  }

  combine <- function(col, r = 0.5) {
    if (length(r) == 1) {
      rgb <- (1 - r) * getRGB() + r * col$getRGB()
      return(color(rgb[1], rgb[2], rgb[3]))
    }
    rgb <- NULL
    for (i in 1:length(r)) {
      rgb <- rbind(rgb, (1 - r[i]) * getRGB() + r[i] * col$getRGB())
    }
    color(rgb[, 1], rgb[, 2], rgb[, 3])
  }

  sequence <- function(col, n) {
    combine(col, if (n != 1) seq(0, 1, 1 / (n - 1)) else 0.5)
  }

  clone <- function() color(r, g, b)

  inverse <- function() color(1 - r, 1 - g, 1 - b)
  return(list(
    getRGB = getRGB, getCol = getCol, getRGBs = getRGBs, getCols = getCols,
    combine = combine, sequence = sequence, clone = clone, inverse = inverse,
    clonable = "clonable.0000000WJ3bLybo9e7EvjO8mFmdiHKTX3lh3BYsPiPst",
    class = "color.000000IM9fKDwnkPL8A0dxaV3zT64Q6cjKifaT0BA9kmcvCAQ1NT"
  ))
}

colorList <- function(r = NA, g = NA, b = NA, maxValue = 1) {
  colist <- newList()
  if (!is.na(r[1]) | !is.na(g[1]) | !is.na(b[1])) {
    if (is.na(r[1])) {
      r <- runif(1)
    } else {
      r <- r / maxValue
    }
    if (is.na(g[1])) {
      g <- runif(1)
    } else {
      g <- g / maxValue
    }
    if (is.na(b[1])) {
      b <- runif(1)
    } else {
      b <- b / maxValue
    }
    n <- max(length(r), length(g), length(b))
    r <- r[1 + ((1:n) - 1) %% length(r)]
    g <- g[1 + ((1:n) - 1) %% length(g)]
    b <- b[1 + ((1:n) - 1) %% length(b)]
    for (i in 1:n) colist$add(color(r[i], g[i], b[i]))
  }
  varIter <- newVec(colist$iter(cyclic = TRUE, color(0, 0, 0)))
  add <- function(col) {
    colist$add(col)
    varIter$set(colist$iter(cyclic = TRUE))
  }

  getColor <- function(this) {
    IF(this, varIter$get()$This(), varIter$get()$Next())
  }

  getRGB <- function(this = F) {
    getColor(this)$getRGB()
  }

  getCol <- function(alp = 1, bri = 1, this = F) {
    getColor(this)$getCol(alp = alp, bri = bri)
  }

  getRGBs <- function() cbind(r, g, b)

  getCols <- function(alp = 1, bri = 1) {
    cls <- colist$iter()
    col <- NULL
    while (cls$HasNext()) {
      col <- rbind(col, cls$Next()$getCols(alp = alp, bri = bri))
    }
    rownames(col) <- paste("color", 1:colist$length(), sep = "")
    col
  }

  clone <- function() color(r, g, b)

  inverse <- function() color(1 - r, 1 - g, 1 - b)

  return(list(
    getRGB = getRGB, getCol = getCol, getRGBs = getRGBs, getCols = getCols,
    add = add, length = colist$length, clone = clone, inverse = inverse,
    clonable = "clonable.0000000WJ3bLybo9e7EvjO8mFmdiHKTX3lh3BYsPiPst",
    class = "color.000000IM9fKDwnkPL8A0dxaV3zT64Q6cjKifaT0BA9kmcvCAQ1NT"
  ))
}

#' Color Combine
#'
#' This function combines multiple colors based on the specified weights, creating a new color.
#'
#' @param ... Colors to combine, specified as color objects.
#' @param r Numeric vector or scalar representing the weights for combining the colors.
#' @param abs Logical value indicating whether the color distances should be absolute (TRUE) or not (FALSE).
#'   Default is FALSE.
#'
#' @return A color object representing the combined color.
#'
#' @examples
#' # Combine two colors with equal weights
#' col1 <- color(r = 0.2, g = 0.4, b = 0.6)
#' col2 <- color(r = 0.8, g = 0.2, b = 0.4)
#' combined_col <- color.combine(col1, col2, r = 0.5)
#' combined_col$getRGB() # Get the RGB values of the combined color
#' combined_col$getCol() # Get the combined color as an R color object
#'
#' # Combine three colors with different weights
#' col3 <- color(r = 0.1, g = 0.5, b = 0.3)
#' combined_col2 <- color.combine(col1, col2, col3, r = c(0.3, 0.5, 0.2))
#' combined_col2$getRGB() # Get the RGB values of the combined color
#' combined_col2$getCol() # Get the combined color as an R color object
#'
#' @export
color.combine <- function(..., r, abs = F) {
  lst <- list(...)
  d <- function(x, y) sqrt(sum((x - y)^2))
  RGB <- list(fixCol(lst[[1]])$getRGB())
  dRGB <- NULL
  for (i in 2:length(lst)) {
    RGB[[i]] <- fixCol(lst[[i]])$getRGB()
    dRGB[i - 1] <- if (abs) 1 else d(RGB[[i - 1]], RGB[[i]])
  }
  dRGB <- cumsum(dRGB)
  dRGB <- c(0, dRGB / dRGB[length(dRGB)])
  n <- length(dRGB)
  retRGB <- NULL
  for (r in r) {
    for (i in 2:n) {
      if (r <= dRGB[i]) {
        ri <- reparam(r, c(dRGB[i - 1], dRGB[i]), c(0, 1))
        retRGB <- rbind(retRGB, (1 - ri) * RGB[[i - 1]] + ri * RGB[[i]])
        break()
      }
    }
  }
  color(retRGB[, 1], retRGB[, 2], retRGB[, 3])
}

#' Color Sequence
#'
#' This function generates a sequence of colors by combining multiple colors based on the specified weights.
#'
#' @param ... Colors to combine, specified as color objects.
#' @param n Number of colors in the sequence.
#' @param abs Logical value indicating whether the color distances should be absolute (TRUE) or not (FALSE).
#'   Default is FALSE.
#' @param inv Logical value indicating whether to invert the sequence of colors. If TRUE, the sequence will be inverted.
#'   Default is FALSE.
#'
#' @return A color object representing the sequence of colors.
#'
#' @examples
#' # Generate a sequence of colors using two base colors
#' col1 <- color(r = 0.2, g = 0.4, b = 0.6)
#' col2 <- color(r = 0.8, g = 0.2, b = 0.4)
#' sequence_col <- color.sequence(col1, col2, n = 5)
#' sequence_col$getRGBs() # Get the RGB values of the color sequence
#' sequence_col$getCols() # Get the color sequence as a matrix of R color objects
#'
#' # Generate an inverted sequence of colors
#' inverted_col <- color.sequence(col1, col2, n = 4, inv = TRUE)
#' inverted_col$getRGBs() # Get the RGB values of the inverted color sequence
#' inverted_col$getCols() # Get the inverted color sequence as a matrix of R color objects
#'
#' @export
color.sequence <- function(..., n, abs = F, inv = F) {
  r <- if (n == 1) 0.5 else seq(0, 1, 1 / (n - 1))
  if (inv) r <- 1 - r
  color.combine(..., r = r, abs = abs)
}

#' Rainbow Colors
#'
#' This function generates a sequence of rainbow colors.
#'
#' @param n Number of colors in the rainbow sequence.
#' @param abs Logical value indicating whether the color distances should be absolute (TRUE) or not (FALSE).
#'   Default is FALSE.
#' @param inv Logical value indicating whether to invert the sequence of colors. If TRUE, the sequence will be inverted.
#'   Default is FALSE.
#'
#' @return A color object representing the rainbow sequence of colors.
#'
#' @examples
#' # Generate a rainbow sequence of colors with 6 colors
#' rainbow_col <- color.rainbow(n = 6)
#' rainbow_col$getRGBs() # Get the RGB values of the rainbow color sequence
#' rainbow_col$getCols() # Get the rainbow color sequence as a matrix of R color objects
#'
#' # Generate an inverted rainbow sequence of colors with 8 colors
#' inverted_rainbow_col <- color.rainbow(n = 8, inv = TRUE)
#' inverted_rainbow_col$getRGBs() # Get the RGB values of the inverted rainbow color sequence
#' inverted_rainbow_col$getCols() # Get the inverted rainbow color sequence as a matrix of R color objects
#'
#' @export
color.rainbow <- function(n, abs = F, inv = F) {
  color.sequence("#FF0000",
    "#FF7F00",
    "#FFFF00",
    "#00FF00",
    "#0000FF",
    "#4B0082",
    "#9400D3",
    n = n, inv = inv, abs = abs
  )
}
