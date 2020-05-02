#' Clean Cost Center Name
#'
#' Removes non-alpha-numeric characters and leading zero in a character vector.
#'
#' @param x character vector
#' @return character vector without leading zero and without non-alpha-numeric
#'   characters
#' @export cleanCC
cleanCC <- function(x) {
  x <- gsub(pattern = "^0*", replacement = "", x = rmS(x))
  x <- gsub(pattern = "[^[:alnum:]]", replacement = "", x = x)
  return(x)
}

#' Expand \code{data.frame}
#'
#' Expands a \code{data.frame} twice its original number of rows. The expanded
#'   rows contain only \code{NA} values.
#'
#' @param x \code{data.frame}
#' @return \code{data.frame} with \code{NA} values at the lower half rows
#' @export expandDF
expandDF <- function(x) {
  tempData <- matrix(NA, nrow = length(x[,1]), ncol = length(x))
  colnames(tempData) <- colnames(x)
  rbind(x, tempData)
}

#' Is capacity of data.frame enough?
#'
#' Tests whether a \code{data.frame} has enough blank rows at the bottom.
#'
#' @param x \code{data.frame}
#' @param dfLength integer representing the number of rows to be appended to x
#' @return \code{TRUE} if number of blank rows is greater than \code{dfLength}.
#'   \code{FALSE} if number of blank rows is less than or equal to
#'   \code{dfLength}.
#' @export capacityEnough
capacityEnough <- function(x, dfLength) {

  remainingSlots <- length(x[,1]) - suppressWarnings(max(which(!is.na(x[,1]))))

  if (is.infinite(remainingSlots))
    remainingSlots <- length(x[,1])

  if (remainingSlots > dfLength) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Test and Expand \code{data.frame}
#'
#' Tests a \code{data.frame} whether there is enough blank rows for appending.
#'   If there is not enough slot, the \code{data.frame} is expanded with blank
#'   rows.
#'
#' @param x \code{data.frame}
#' @param dfLength integer representing the number of rows to be appended to x
#' @return \code{data.frame} with NA rows at the bottom
#' @export testAndExpand
testAndExpand <- function(x, dfLength) {
  while (!capacityEnough(x, dfLength))
    x <- expandDF(x)
  return(x)
}

#' Append data.frame
#'
#' Append \code{data.frame} y to \code{data.frame} x.
#'
#' @param x \code{data.frame} which may contain NA rows at the bottom
#' @param y \code{data.frame}
#' @return \code{data.frame} which may contain NA rows at the bottom
#' @export dfAppend
dfAppend <- function(x, y) {

  lengthY <- length(y[,1])
  x       <- testAndExpand(x = x, dfLength = lengthY)

  if (is.infinite(suppressWarnings(max(which(!is.na(x[,1])))))) {
    x[1:lengthY,] <- y
  } else {

    tempIndex <- 1L + max(which(!is.na(x[,1])))

    x[(tempIndex):(tempIndex + lengthY - 1),] <- y
  }

  return(x)
}

#' Remove white space
#'
#' Spaces with more than 2-character length are removed and are replaced with 1
#'   space. Trailing and leading spaces are also removed.
#'
#' @param x character vector
#' @return character vector without 2 adjacent space characters
#' @export rmWS
rmWS <- function(x) {
  x <- gsub(pattern = "\\s+", replacement = " ", x = x)
  x <- trimws(x)
  return(x)
}

#' Remove space
#'
#' Spaces character is removed from all elements of the character vector.
#'
#' @param x character vector
#' @return character vector without 2 adjacent space characters
#' @export rmS
rmS <- function(x) {
  x <- gsub(pattern = "\\s+", replacement = "", x = x)
  return(x)
}

#' Remove leading zero
#'
#' Remove leading zeroes for purely numeric characters.
#'
#' @param x character vector
#' @return character vector with leading zero removed
#' @export rmLead0
rmLead0 <- function(x) {
  y <- rep(NA, times = length(x))
  for (i in 1:length(x)) {
    tryCatch(
      {
        j <- as.character(as.integer(j <- x[i]))
      },
      warning = function(w) {
        j <- j <- x[i]
      },
      finally = {
        y[i] <- j
        j <- NULL
      }
    )
  }
  y
}
