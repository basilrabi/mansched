#' Sanity Check for Employee Requirement Sheet
#'
#' @param empReq see \link{initEmpReq}
#' @return empReq with the the following changes:
#'   \enumerate{
#'     \item \code{spareFactor} converted to numeric
#'     \item \code{quantity} and \code{OT} changed to integer
#'     \item \code{NA} and0 quantities removed
#'   }
#' @export sanityCheckEmpReq
sanityCheckEmpReq <- function(empReq) {
  empReq.colnames <- c("activity",
                       "personnelClass",
                       "quantity",
                       "spareFactor",
                       "equipment",
                       "OT",
                       "costCenter")

  empReq <- empReq[, empReq.colnames]

  # Check for data types

  if (length(empReq) != length(empReq.colnames)) {
    stop(paste("Column names of Requirement must be:",
               paste(empReq.colnames, collapse = " ")))
  }

  if (class(empReq$activity) != "character")
    stop("Column activity in Requirement is not character!")

  if (class(empReq$personnelClass) != "character")
    stop("Column personnelClass in Requirement is not character!")

  if (class(empReq$quantity) != "numeric")
    stop("Column quantity in Requirement is not numeric!")

  if (!all(is.na(empReq$spareFactor)) & class(empReq$spareFactor) != "numeric")
    stop("Column spareFactor in Requirement is not numeric!")

  empReq$spareFactor <- as.numeric(empReq$spareFactor)

  if (class(empReq$equipment) != "character")
    stop("Column equipment in Requirement is not character!")

  if (class(empReq$OT) != "numeric")
    stop("Column OT in Requirement is not numeric!")

  if (class(empReq$costCenter) != "character")
    stop("Column costCenter in Requirement is not character!")

  if (any(is.na(empReq$costCenter)))
    stop("Requirement without cost center detected!")

  empReq[, c("quantity", "OT")] <- lapply(empReq[, c("quantity", "OT")],
                                          FUN = as.integer)
  empReq <- as.data.frame(empReq)

  # Remove empReq rows with zero or NA quantities
  empReq <- empReq[!is.na(empReq$quantity),]
  empReq <- empReq[!empReq$quantity == 0,]
  return(empReq)
}
