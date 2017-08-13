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
#' A character vector containing the valid equipment types in Taganito Mine.
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

#' Valid \code{personnelClass}
#'
#' A character vector containing the valid personnel classes used in this
#'   package.
#'
#' @format character vector
#' @export validEmpClass
validEmpClass <- c("division manager",
                   "group manager",
                   "department manager",
                   "section head",
                   "clerk",
                   "technical",
                   "supervisor",
                   "laborer",
                   "operator")

#' Valid employment \code{status}
#'
#' A character vector containing the valid employment status in Taganito Mine.
#'
#' @format character vector
#' @export validEmpStatus
validEmpStatus <- c("reg", "pro", "sea")

#' Payment scheme for regular non-RF
#'
#' Salary increase of a regular non-RF employee is effective on July 1.
#'
#' @format \code{\link{data.frame}} with 12 rows and two columns
#' @export payA
payA <- data.frame(month = 1:12,
                   sal = c(rep("a", times = 6),
                           rep("b", times = 6)),
                   stringsAsFactors = FALSE)

#' Payment scheme for regular RF
#'
#' Salary increase of a regular RF employee is effective on February 1
#'
#' @format \code{\link{data.frame}} with 12 rows and two columns
#' @export payB
payB <- data.frame(month = 1:12,
                   sal = c("a",
                           rep("b", times = 11)),
                   stringsAsFactors = FALSE)

#' Wage premiums
#'
#' Hourly salary multipliers according to man hour types along with the thier
#'   corresponding night premiums.
#'
#' @format \code{\link{data.frame}} with 16 rows and columns
#'
#'   Each row represents the premium pay for a man hour type. The columns are
#'   cosisted by:
#'   \describe{
#'     \item{type}{man hour type (see \code{\link{assignEmp}})}
#'     \item{premiumR}{wage premium enjoyed by regular employees}
#'     \item{premiumS}{wage premium enjoyed by non-regular employees}
#'     \item{npR}{night premium enjoyed by regular employees}
#'     \item{npS}{night premium enjoyed by non-regular employees}
#'   }
#' @export premium
premium <- data.frame(type = c("reg",
                               "regOT",
                               "rd",
                               "rdOT",
                               "sh",
                               "shOT",
                               "lh",
                               "lhOT",
                               "nh",
                               "nhOT",
                               "rs",
                               "rsOT",
                               "rl",
                               "rlOT",
                               "rn",
                               "rnOT"),
                      premiumR = c(1.000,
                                   1.350,
                                   1.600,
                                   2.050,
                                   1.600,
                                   2.050,
                                   2.100,
                                   2.700,
                                   2.100,
                                   2.700,
                                   2.350,
                                   3.025,
                                   3.100,
                                   4.000,
                                   3.100,
                                   4.000),
                      premiumS = c(1.000,
                                   1.250,
                                   1.300,
                                   1.690,
                                   1.300,
                                   1.690,
                                   2.000,
                                   2.600,
                                   1.000,
                                   1.250,
                                   1.690,
                                   2.200,
                                   2.600,
                                   3.380,
                                   1.300,
                                   1.690),
                      npR = c(0.200,
                              0.200,
                              0.230,
                              0.230,
                              0.230,
                              0.230,
                              0.460,
                              0.460,
                              0.460,
                              0.460,
                              0.230,
                              0.230,
                              0.460,
                              0.460,
                              0.460,
                              0.460),
                      npS = c(0.100,
                              0.100,
                              0.130,
                              0.130,
                              0.130,
                              0.130,
                              0.200,
                              0.200,
                              0.100,
                              0.100,
                              0.130,
                              0.130,
                              0.200,
                              0.200,
                              0.130,
                              0.130),
                      stringsAsFactors = FALSE)
