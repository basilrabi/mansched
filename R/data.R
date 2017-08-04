#' Holidays in Taganito, Claver in the year 2017
#'
#' A data-set containing the dates of holidays and their descriptions.
#'
#' @format \code{\link{data.frame}} with 25 rows and 4 columns:
#'   \describe{
#'     \item{Month}{month name}
#'     \item{Day}{day in the month}
#'     \item{Type}{type of holiday.}
#'     \item{Description}{description of holiday}
#'   }
"holidays"

#' Valid equipment types
#'
#' A character vector containing the valid equipment types in Taganito Mine
#'
#' @format character vector:
#'
#'   \describe{
#'     \item{CRANE}{crane}
#'     \item{CT}{bull dozer}
#'     \item{DT}{dump truck}
#'     \item{FL}{fuel truck}
#'     \item{FORKLIFT}{forklift}
#'     \item{RG}{road grader}
#'     \item{SB}{service bus}
#'     \item{SP}{service pickup}
#'     \item{TT}{prime mover (trailer truck)}
#'     \item{TX}{tracked excavator}
#'     \item{VC}{vibrating compactor}
#'     \item{WL}{wheeled payloader}
#'     \item{WTL}{water truck}
#'     \item{WX}{wheeled excavator}
#'   }
#' @export validEquipment
validEquipment <- c("CRANE",
                    "CT",
                    "DT",
                    "FL",
                    "FORKLIFT",
                    "RG",
                    "SB",
                    "SP",
                    "TT",
                    "TX",
                    "VC",
                    "WL",
                    "WTL",
                    "WX")
