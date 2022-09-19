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

#' Cost accounting codes in SAP
#'
#' SAP cost codes used by accounting.
#'
#' @format \code{\link{data.frame}} with 779 rows and 3 columns:
#'   \describe{
#'     \item{code}{accounting code}
#'     \item{description}{cost component}
#'     \item{approach}{where to distribute SAP cost to in-house cost centers}
#'   }
"acSAP"

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
validEmpStatus <- c("reg", "pro", "sea", "age")

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

seasonalSigningBonus <- 2400

#' Employer's SSS Contribution
#'
#' Details are posted in
#' \url{https://www.sss.gov.ph/sss/appmanager/pages.jsp?page=scheduleofcontribution}.
#'
#' @export SSS
SSS <- data.frame(r1 = c(-1,
                         3250,
                         3750,
                         4250,
                         4750,
                         5250,
                         5750,
                         6250,
                         6750,
                         7250,
                         7750,
                         8250,
                         8750,
                         9250,
                         9750,
                         10250,
                         10750,
                         11250,
                         11750,
                         12250,
                         12750,
                         13250,
                         13750,
                         14250,
                         14750,
                         15250,
                         15750,
                         16250,
                         16750,
                         17250,
                         17750,
                         18250,
                         18750,
                         19250,
                         19750,
                         20250,
                         20750,
                         21250,
                         21750,
                         22250,
                         22750,
                         23250,
                         23750,
                         24250,
                         24750),
                  r2 = c(3249.99,
                         3749.99,
                         4249.99,
                         4749.99,
                         5249.99,
                         5749.99,
                         6249.99,
                         6749.99,
                         7249.99,
                         7749.99,
                         8249.99,
                         8749.99,
                         9249.99,
                         9749.99,
                         10249.99,
                         10749.99,
                         11249.99,
                         11749.99,
                         12249.99,
                         12749.99,
                         13249.99,
                         13749.99,
                         14249.99,
                         14749.99,
                         15249.99,
                         15749.99,
                         16249.99,
                         16749.99,
                         17249.99,
                         17749.99,
                         18249.99,
                         18749.99,
                         19249.99,
                         19749.99,
                         20249.99,
                         20749.99,
                         21249.99,
                         21749.99,
                         22249.99,
                         22749.99,
                         23249.99,
                         23749.99,
                         24249.99,
                         24749.99,
                         999999999),
                  c = c(265.00,
                        307.50,
                        350.00,
                        392.50,
                        435.00,
                        477.50,
                        520.00,
                        562.50,
                        605.00,
                        647.50,
                        690.00,
                        732.50,
                        775.00,
                        817.50,
                        860.00,
                        902.50,
                        945.00,
                        987.50,
                        1030.00,
                        1072.50,
                        1115.00,
                        1157.50,
                        1200.00,
                        1242.50,
                        1305.00,
                        1347.50,
                        1390.00,
                        1432.50,
                        1475.00,
                        1517.50,
                        1560.00,
                        1602.50,
                        1645.00,
                        1687.50,
                        1730.00,
                        1772.50,
                        1815.00,
                        1857.50,
                        1900.00,
                        1942.50,
                        1985.00,
                        2027.50,
                        2070.00,
                        2112.50,
                        2155.00))

#' Costs accounting codes in M3
#'
#' M3 codes used by accounting.
#'
#' @export ac
ac <- data.frame(row = c("Salaries-Regular",
                         "OT Pay - Regular",
                         "Salaries-Seasonal",
                         "OT Pay - Seasonal",
                         "Employees Allowance",
                         "Employee Benefits",
                         "Premium SSS, EC",
                         "Prem-HDMF (Pag-ibig)",
                         "Philhealth",
                         "Leave Commutation",
                         "Hospital and Medical Expenses",
                         "Bonus",
                         "man-hours",
                         "CF Others",
                         "CF Manpower Services",
                         "Safety Gadgets",
                         "Grouplife",
                         "HMO",
                         "Food Allowance / Rice Subsidy",
                         "Retirement Benefits",
                         "Food and Food Supplies",
                         "Transportation Expense",
                         "Representation Expense",
                         "Office Supplies",
                         "Depletion Expense"),
                 code = c(521001L,
                          521021L,
                          521002L,
                          521022L,
                          521004L,
                          521005L,
                          521006L,
                          521007L,
                          521008L,
                          521012L,
                          521017L,
                          521009L,
                          999999L,
                          522099L,
                          522010L,
                          523011L,
                          524037L,
                          521018L,
                          521011L,
                          521030L,
                          523010L,
                          524002L,
                          524003L,
                          524030L,
                          525002L))
