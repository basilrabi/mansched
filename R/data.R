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

#' General ledger codes in SAP
#'
#' SAP general ledger accounts used by accounting.
#'
#' @format \code{\link{data.frame}} with 783 rows and 5 columns:
#'   \describe{
#'     \item{code}{accounting code}
#'     \item{description}{cost component}
#'     \item{approach}{how to distribute SAP cost to old cost centers}
#'     \item{activity_group}{flag whether the cost can be distributed to in-house (I), rental (R), or contract (C) old cost centers}
#'     \item{employee_type}{employment type of employee if cost is to be distributed by manhours}
#'     \item{mh_type}{type of manhours whether regular or overtime}
#'   }
"acSAP"

#' Employer's SSS Contribution
#'
#' Details are posted in
#' \url{https://www.sss.gov.ph/wp-content/uploads/2024/12/2025-SSS-Contribution-Table-rev.pdf}.
#'
#' @format \code{\link{data.frame}} with 62 rows and 3 columns:
#'   \describe{
#'     \item{r1}{minimum gross salary}
#'     \item{r2}{maximum gross salary}
#'    }
"SSS"

#' Valid equipment types
#'
#' A character vector containing the valid equipment types in Taganito Mine.
#'
#' @format character vector:
#'
#'   \describe{
#'     \item{ADT}{articulated dump truck}
#'     \item{BREAKER}{breaker}
#'     \item{CRANE}{crane}
#'     \item{CT}{bull dozer}
#'     \item{DT}{dump truck}
#'     \item{FL}{fuel truck}
#'     \item{FORKLIFT}{forklift}
#'     \item{HDT}{HOWO dump truck}
#'     \item{MDT}{mini dump truck}
#'     \item{MOC}{mobile crusher}
#'     \item{MOS}{mobile screen}
#'     \item{MTX}{mini tracked excavator}
#'     \item{MWL}{mini wheeled pay loader}
#'     \item{PB}{pump boat}
#'     \item{PMT}{prime mover (trailer)}
#'     \item{RADIO}{radio}
#'     \item{RG}{road grader}
#'     \item{SB}{service bus}
#'     \item{SP}{service pickup}
#'     \item{ST}{service truck}
#'     \item{TX}{tracked excavator}
#'     \item{VC}{vibrating compactor}
#'     \item{WL}{wheeled pay loader}
#'     \item{WTL}{water truck}
#'     \item{WX}{wheeled excavator}
#'   }
#' @export validEquipment
validEquipment <- c("ADT",
                    "BREAKER",
                    "CRANE",
                    "CT",
                    "DT",
                    "FL",
                    "FORKLIFT",
                    "HDT",
                    "MDT",
                    "MOC",
                    "MOS",
                    "MTX",
                    "MWL",
                    "PB",
                    "PMT",
                    "RADIO",
                    "RG",
                    "SB",
                    "SP",
                    "ST",
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
validEmpClass <- c("divisionmanager",
                   "groupmanager",
                   "departmentmanager",
                   "sectionhead",
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
validEmpStatus <- c("reg", "pro", "sea", "age", "prj")

#' Payment scheme for regular non-RF
#'
#' Salary increase of a regular non-RF employee is effective on July 1.
#'
#' @format \code{\link{data.frame}} with 12 rows and two columns
#' @export payA
payA <- data.frame(month = 1:12,
                   sal = c(rep("a", times = 0), rep("b", times = 12)))

#' Payment scheme for regular RF
#'
#' Salary increase of a regular RF employee is effective on February 1
#'
#' @format \code{\link{data.frame}} with 12 rows and two columns
#' @export payB
payB <- data.frame(month = 1:12,
                   sal = c("a", rep("b", times = 11)))

#' Wage premiums
#'
#' Hourly salary multipliers according to man hour types along with the their
#'   corresponding night premiums.
#'
#' @format \code{\link{data.frame}} with 16 rows and 7 columns
#'
#'   Each row represents the premium pay for a man hour type. The columns are
#'   consisted by:
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
                                 TRUE))

#' Wage premiums for Rank and File
#'
#' @seealso \code{\link{premium}} \code{\link{premium.nonRF}}
#' @export premium.RF
premium.RF <- premium
iR <- premium.RF$mhType %in% c("sh", "lh", "nh", "rs", "rl", "rn")
iS <- premium.RF$mhType %in% c("sh", "lh", "rs", "rl")
premium.RF[iR, c("premiumR")] <-  premium.RF[iR, c("premiumR")] - 1
premium.RF[iS, c("premiumS")] <-  premium.RF[iS, c("premiumS")] - 1

#' Wage premiums for non-Rank and File
#'
#' @seealso \code{\link{premium}} \code{\link{premium.RF}}
#' @export premium.nonRF
premium.nonRF <- premium
iR            <- premium.nonRF$mhType %in% c("sh", "lh", "nh")
iS            <- premium.nonRF$mhType %in% c("sh", "lh")
premium.nonRF[iR, c("premiumR")] <- premium.nonRF[iR, c("premiumR")] - 1
premium.nonRF[iS, c("premiumS")] <- premium.nonRF[iS, c("premiumS")] - 1

monthRegularSigningBonus <- 2L
seasonalSigningBonus <- 0
