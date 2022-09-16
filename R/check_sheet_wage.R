#' Sanity Check for Wage Sheet
#'
#' @param wage see \link{getCost}
#' @param empPool see \link{initEmpPool}
#' @return wage with
#' @export sanityCheckWage
sanityCheckWage <- function(wage, empPool) {
  wage.colnames <- c("ID", "S", "s", "I", "i")

  # The salary column can either be `S` or `s`
  wage <- wage[, colnames(wage) %in% wage.colnames]
  colnames(wage)[2] <- "s"
  colnames(wage)[3] <- "i"

  if (!is(wage$ID, "character"))
    stop("Column ID in wage is not character!")

  if (!is(wage$s, "numeric"))
    wage$s <- as.numeric(wage$s)

  if (!is(wage$i, "numeric"))
    wage$i <- as.numeric(wage$i)

  wage <- wage[wage$ID %in% empPool$ID,]
  wage$s[is.na(wage$s)] <- 0
  wage$i[is.na(wage$i)] <- 0
  wage <- as.data.frame(wage)
  return(wage)
}
