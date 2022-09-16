#' Sanity Check for Holiday Sheet
#'
#' @param hol see \link{getmhDB}
#' @return hol with \code{Day} converted to integer
#' @export sanityCheckHol
sanityCheckHol <- function(hol) {
  hol.colnames <- c("Month", "Day", "Type", "Description")
  hol <- hol[, hol.colnames]
  # Check data types

  if (length(hol) != length(hol.colnames)) {
    stop(paste("Column names of hol must be:",
               paste(hol.colnames, collapse = " ")))
  }

  if (!is(hol$Month, "character"))
    stop("Column Month in hol is not character!")

  if (!is(hol$Day, "numeric"))
    stop("Column Day in hol is not numeric!")

  hol$Day <- as.integer(hol$Day)

  if (!is(hol$Type, "character"))
    stop("Column Type in hol is not character!")

  if (!is(hol$Description, "character"))
    stop("Column Description in hol is not character!")

  hol <- as.data.frame(hol)
  return(hol)
}
