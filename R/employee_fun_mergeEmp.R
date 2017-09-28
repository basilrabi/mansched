#' Merge theoretical employee list
#'
#' Combines all man hour types of a list of theoretical employees.
#'   All man hours are stored in only one theoretical employee.
#'
#' @param x a list of \code{\link{Employee-class}} representing theoretical
#'   employees with the same class and the same costCode slot
#'
#'   For \code{\link{Operator-class}}, the equipment slot must also be the same.
#' @return an \code{\link{Employee-class}} object
#' @export mergeEmp
mergeEmp <- function(x) {

  if(class(x) != "list")
    stop("Argument is not a list of Employee-class objects!")

  if (length(x) == 1L) {

    if (is(x[[1]], "Employee")) {

      x[[1]]@ID <- paste(class(x[[1]]), x[[1]]@costCode, sep = "-")

      if (is(x[[1]], "Operator"))
        x[[1]]@ID <- paste(x[[1]]@ID, x[[1]]@equipment, sep = "-")

      return(x[[1]])

    } else {

      stop("Argument is not a list of Employee-class objects!")

    }

  }

  if (!is(x[[1]], "Employee"))
    stop("Argument is not a list of Employee-class objects!")

  tempEmp <- x[[1]]

  tempClass <- sapply(x, FUN = function(x) {class(x)})
  tempClass <- unique(tempClass)
  if (length(tempClass) > 1)
    stop("Incompatible Employee-class!")

  tempCostCode <- sapply(x, FUN = function(x) {x@costCode})
  tempCostCode <- unique(tempCostCode)

  if (length(tempCostCode) > 1)
    stop("Incompatible cost code!")

  tempEmp@ID <- paste(tempClass, tempCostCode, sep = "-")

  if (is(x[[1]], "Operator")) {

    tempEquip <- sapply(x, FUN = function(x) {x@equipment})
    tempEquip <- unique(tempEquip)
    if (length(tempEquip) > 1)
      stop("Incompatible equipment!")

    tempEmp@ID <- paste(tempEmp@ID, tempEquip, sep = "-")

  }

  zero <- rep(0L, times = 12)

  reg <- sapply(x, FUN = function(y) {y@reg})
  reg <- apply(reg, MARGIN = 1, FUN = sum)
  tempEmp@reg <- reg

  if (is(tempEmp, "Non Staff")) {

    regOT <- sapply(x, FUN = function(y) {y@regOT})
    regOT <- apply(regOT, MARGIN = 1, FUN = sum)
    tempEmp@regOT <- regOT

  }

  if (is(tempEmp, "Operation Personnel")) {

    sh <- sapply(x, FUN = function(y) {y@sh})
    lh <- sapply(x, FUN = function(y) {y@lh})
    nh <- sapply(x, FUN = function(y) {y@nh})
    shOT <- sapply(x, FUN = function(y) {y@shOT})
    lhOT <- sapply(x, FUN = function(y) {y@lhOT})
    nhOT <- sapply(x, FUN = function(y) {y@nhOT})

    sh <- apply(sh, MARGIN = 1, FUN = sum)
    lh <- apply(lh, MARGIN = 1, FUN = sum)
    nh <- apply(nh, MARGIN = 1, FUN = sum)
    shOT <- apply(shOT, MARGIN = 1, FUN = sum)
    lhOT <- apply(lhOT, MARGIN = 1, FUN = sum)
    nhOT <- apply(nhOT, MARGIN = 1, FUN = sum)

    tempEmp@sh <- sh
    tempEmp@lh <- lh
    tempEmp@nh <- nh
    tempEmp@shOT <- shOT
    tempEmp@lhOT <- lhOT
    tempEmp@nhOT <- nhOT

  }

  return(tempEmp)
}
