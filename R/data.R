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
#' Hourly salary multipliers according to man hour types along with the their
#'   corresponding night premiums.
#'
#' @format \code{\link{data.frame}} with 16 rows and 7 columns
#'
#'   Each row represents the premium pay for a man hour type. The columns are
#'   cosisted by:
#'   \describe{
#'     \item{mhType}{character value representing the man hour type (see
#'       \code{\link{assignEmp}})}
#'     \item{premiumR}{numeric value representing the wage premium enjoyed by
#'       regular employees}
#'     \item{premiumS}{numeric value representing wage premium enjoyed by
#'       non-regular employees}
#'     \item{npR}{numeric value representing night premium enjoyed by regular
#'       employees}
#'     \item{npS}{numeric value representing night premium enjoyed by
#'       non-regular employees}
#'     \item{isOT.R}{logical value \cr
#'       Is man hour type of regular employee treated as overtime work?}
#'     \item{isOT.S}{logical value \cr
#'       Is man hour type of non-regular employee treated as overtime work?}
#'
#'   }
#' @seealso \code{\link{premium.RF}} \code{\link{premium.nonRF}}
#' @export premium
premium <- data.frame(mhType = c("reg",
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
                                   2.500,
                                   2.350,
                                   3.025,
                                   3.100,
                                   4.000,
                                   2.600,
                                   3.350),
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
                                   3.375,
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
                      isOT.R = c(FALSE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE),
                      isOT.S = c(FALSE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 FALSE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE,
                                 TRUE),
                      stringsAsFactors = FALSE)

#' Wage premiums for Rank and File
#'
#' @seealso \code{\link{premium}} \code{\link{premium.nonRF}}
#' @export premium.RF
premium.RF <- premium
premium.RF[which(premium.RF$mhType %in% c("sh",
                                          "lh",
                                          "nh",
                                          "rs",
                                          "rl",
                                          "rn")),
           c("premiumR")] <-
  premium.RF[which(premium.RF$mhType %in% c("sh",
                                            "lh",
                                            "nh",
                                            "rs",
                                            "rl",
                                            "rn")),
             c("premiumR")] - 1
premium.RF[which(premium.RF$mhType %in% c("sh",
                                          "lh",
                                          "rs",
                                          "rl")),
           c("premiumS")] <-
  premium.RF[which(premium.RF$mhType %in% c("sh",
                                            "lh",
                                            "rs",
                                            "rl")),
             c("premiumS")] - 1

#' Wage premiums for non-Rank and File
#'
#' @seealso \code{\link{premium}} \code{\link{premium.RF}}
#' @export premium.nonRF
premium.nonRF <- premium
premium.nonRF[which(premium.nonRF$mhType %in% c("sh",
                                                "lh",
                                                "nh")),
              c("premiumR")] <-
  premium.nonRF[which(premium.nonRF$mhType %in% c("sh",
                                                  "lh",
                                                  "nh")),
                c("premiumR")] - 1
premium.nonRF[which(premium.nonRF$mhType %in% c("sh",
                                                "lh")),
              c("premiumS")] <-
  premium.nonRF[which(premium.nonRF$mhType %in% c("sh",
                                                  "lh")),
                c("premiumS")] - 1
