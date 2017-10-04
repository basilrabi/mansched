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
#'     \item{MOC}{Mobile Crusher}
#'     \item{MOS}{Mobile Screen}
#'     \item{PB}{Pump boat}
#'     \item{RG}{road grader}
#'     \item{SB}{service bus}
#'     \item{SP}{service pickup}
#'     \item{ST}{service truck}
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
                    "MOC",
                    "MOS",
                    "PB",
                    "RG",
                    "SB",
                    "SP",
                    "ST",
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
validEmpStatus <- c("reg", "pro", "sea", "age")

#' Payment scheme for regular non-RF
#'
#' Salary increase of a regular non-RF employee is effective on July 1.
#'
#' @format \code{\link{data.frame}} with 12 rows and two columns
#' @export payA
payA <- data.frame(month            = 1:12,
                   sal              = c(rep("a", times = 6),
                                        rep("b", times = 6)),
                   stringsAsFactors = FALSE)

#' Payment scheme for regular RF
#'
#' Salary increase of a regular RF employee is effective on February 1
#'
#' @format \code{\link{data.frame}} with 12 rows and two columns
#' @export payB
payB <- data.frame(month            = 1:12,
                   sal              = c("a", rep("b", times = 11)),
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


#' Employer's SSS Contribution
#'
#' Details are posted in
#' \url{https://www.sss.gov.ph/sss/appmanager/pages.jsp?page=scheduleofcontribution}.
#'
#' @export SSS
SSS <- data.frame(r1 = c(-1,
                         1000,
                         1250,
                         1750,
                         2250,
                         2750,
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
                         15750),
                  r2 = c(1000,
                         1249.99,
                         1749.99,
                         2249.99,
                         2749.99,
                         3249.99,
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
                         999999999),
                  c = c(0,
                        83.70,
                        120.50,
                        157.30,
                        194.20,
                        231.00,
                        267.80,
                        304.70,
                        341.50,
                        378.30,
                        415.20,
                        452.00,
                        488.80,
                        525.70,
                        562.50,
                        599.30,
                        636.20,
                        673.00,
                        709.80,
                        746.70,
                        783.50,
                        820.30,
                        857.20,
                        894.00,
                        930.80,
                        967.70,
                        1004.50,
                        1041.30,
                        1078.20,
                        1135.00,
                        1171.80,
                        1208.70))

#' Employer's PHIC Contribution
#'
#' Details are posted in
#' \url{https://www.philhealth.gov.ph/partners/employers/contri_tbl.html}.
#'
#' @export PHIC
PHIC <- data.frame(r1 = c( -1.00,
                           1.00,
                           9000.00,
                           10000.00,
                           11000.00,
                           12000.00,
                           13000.00,
                           14000.00,
                           15000.00,
                           16000.00,
                           17000.00,
                           18000.00,
                           19000.00,
                           20000.00,
                           21000.00,
                           22000.00,
                           23000.00,
                           24000.00,
                           25000.00,
                           26000.00,
                           27000.00,
                           28000.00,
                           29000.00,
                           30000.00,
                           31000.00,
                           32000.00,
                           33000.00,
                           34000.00,
                           35000.00),
                   r2 = c( 1.00,
                           8999.99,
                           9999.99,
                           10999.99,
                           11999.99,
                           12999.99,
                           13999.99,
                           14999.99,
                           15999.99,
                           16999.99,
                           17999.99,
                           18999.99,
                           19999.99,
                           20999.99,
                           21999.99,
                           22999.99,
                           23999.99,
                           24999.99,
                           25999.99,
                           26999.99,
                           27999.99,
                           28999.99,
                           29999.99,
                           30999.99,
                           31999.99,
                           32999.99,
                           33999.99,
                           34999.99,
                           999999999),
                   c = c(0,
                         100.00,
                         112.50,
                         125.00,
                         137.50,
                         150.00,
                         162.50,
                         175.00,
                         187.50,
                         200.00,
                         212.50,
                         225.00,
                         237.50,
                         250.00,
                         262.50,
                         275.00,
                         287.50,
                         300.00,
                         312.50,
                         325.00,
                         337.50,
                         350.00,
                         362.50,
                         375.00,
                         387.50,
                         400.00,
                         412.50,
                         425.00,
                         437.50))

#' Employee costs accounting codes
#'
#' M3 codes used by accounting for employee costs.
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
                         "13th Month Pay",
                         "man-hours",
                         "CF Others"),
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
                          522099L),
                 stringsAsFactors = FALSE)
