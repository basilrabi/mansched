#' Sanity Check for Employee Pool Sheet
#'
#' @param empPool see \link{initEmpPool}
#' @return empPool with the columns cBegin and cEnd converted to characters.
#' @export sanityCheckEmpPool
sanityCheckEmpPool <- function(empPool) {
  dependentsCol <- month.abb %>% toupper()
  attendanceCol <- paste0("a_", 1:12)
  d.rdCol       <- paste0("d.rd_", 1:12)
  d.hoCol       <- paste0("d.ho_", 1:12)
  d.rhCol       <- paste0("d.rh_", 1:12)
  empPool.colnames <- c("ID",
                        "name",
                        "designation",
                        "personnelClass",
                        "equipment",
                        "costCenter",
                        "status",
                        "cBegin",
                        "cEnd",
                        "inHouse",
                        "restday",
                        "isRF",
                        d.rdCol,
                        d.hoCol,
                        d.rhCol,
                        "dcc",
                        "field",
                        dependentsCol,
                        attendanceCol,
                        "VL",
                        "SL")

  empPool <- empPool[, empPool.colnames]

  # Check for data types

  if (length(empPool) != length(empPool.colnames)) {
    stop(paste("Column names of Pool must be:",
               paste(empPool.colnames, collapse = " ")))
  }

  if (class(empPool$ID) != "character")
    stop("Column ID in Pool is not character!")

  if (class(empPool$name) != "character")
    stop("Column name in Pool is not character!")

  if (class(empPool$designation) != "character")
    stop("Column designation in Pool is not character!")

  if (class(empPool$personnelClass) != "character")
    stop("Column personnelClass in Pool is not character!")

  if (!all(is.na(empPool$equipment)) & class(empPool$equipment) != "character")
    stop("Column equipment in Pool is not character!")

  if (!all(is.na(empPool$equipment)) & class(empPool$costCenter) != "character")
    stop("Column costCenter in Pool is not character!")

  if (!all(is.na(empPool$dcc)) & class(empPool$dcc) != "character")
    stop("Column dcc in Pool is not character!")

  if (class(empPool$status) != "character")
    stop("Column status in Pool is not character!")

  if (!"POSIXct" %in% class(empPool$cBegin))
    stop("Column cBegin in Pool is not date!")

  if (!"POSIXct" %in% class(empPool$cEnd))
    stop("Column cEnd in Pool is not date!")

  if (class(empPool$restday) != "character")
    stop("Column restday in Pool is not character!")

  for (i in dependentsCol) {
    if (class(empPool[[i]]) != "numeric") {
      stop(paste("Column", i, "is not numeric!"))
    }
  }

  for (i in c(attendanceCol, d.rdCol, d.hoCol, d.rhCol)) {
    if (class(empPool[[i]]) != "numeric")
      stop(paste("Column", i, "is not numeric!"))

    if (anyNA(empPool[[i]]))
      stop(paste("Column", i, "is incomplete!"))

  }

  if (class(empPool$VL) != "numeric")
    stop("Column VL is not numeric!")
  if (class(empPool$SL) != "numeric")
    stop("Column SL is not numeric!")

  empPool[, c("cBegin", "cEnd")] <- lapply(empPool[, c("cBegin", "cEnd")],
                                           FUN = as.character)
  empPool <- as.data.frame(empPool)
  empPool$field <- as.logical(empPool$field)
  return(empPool)
}
