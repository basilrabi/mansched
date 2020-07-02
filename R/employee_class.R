#' @import methods
NULL

#' An S4 class defining an employee
#'
#' This class is based on the organizational structure of Taganito Mining
#'   Corporation's (TMC) Mines Group. This can be used in representing
#'   both real and theoretical employees.
#'
#' Real employees are actual persons represented as \code{Employee-class}
#'   objects. Theoretical employees in this context are employee requirements
#'   represented as \code{Employee-class} objects.
#'
#'   This class is further classified into \code{\link{Staff-class}} and
#'   \code{\link{NonStaff-class}}.
#'
#' @slot ID character string defining the employee's unique identifier
#' @slot name character string defining the name of employee
#' @slot designation character string defining the designation of the
#'   employee
#' @slot attendance integer vector with length of 12 estimating the attendance
#'   rate of the employee for the whole year.
#'
#'   This defaults to 1 for theoretical employees. For real employees,
#'   a value of not more than 1 but greater than 0 may be used.
#' @slot spareFactor numeric value estimating the man power spare factor for
#'   an activity
#'
#'   This is usually used as man hours multiplier when a spare operator is
#'   assigned. The spare factor defaults to 1 for real employees. For
#'   theoretical employees, a value greater than 1 may be used.
#' @slot costCenter character vector containing the cost centers wherein the
#'   employee will be charged
#' @slot status character string defining the employment status of the employee
#' @slot cBegin character string defining the date of start of employment of the
#'   employee
#' @slot cEnd character string defining the date of end of employment of the
#'   employee
#' @slot inHouse logical value \cr
#'   Is the employee's accommodation given by the company?
#' @slot restday character string defining the day of the week wherein the
#'   employee is not required to report to work
#' @slot holHours integer vector with length of 12
#'
#'   This represents the total number of hours consisting of only the first
#'   eight hours of the holidays wherein the employee is still paid even if the
#'   employee will not report to work. Each element of the vector represents a
#'   month.
#'
#'   For regular non-RF employees, \code{holHours} is comprised
#'   by special holidays, legal holidays, and negotiated holidays. For
#'   non-RF non-Regular employees, \code{holHours} is comprised by legal
#'   holidays and special holidays.
#'
#'   For regular RF employees, \code{holHours} is comprised by special holidays,
#'   legal holidays, negotiated holidays, special holidays on rest days, legal
#'   holidays on rest days, and negotiated holidays on rest days. For
#'   non-regular RF employees, \code{holHours} is comprised by legal holidays,
#'   legal holidays on rest days, worked special holidays, and worked special
#'   holidays on rest days..
#'
#'   This slot only applies to real employees.
#' @slot maxReg integer vector with length of 12
#'
#'   This represents the number of hours wherein the employee must report to
#'   work in order to be compensated. Regular employees may not report to work
#'   during any type of holiday. Non-regular employees, on the other hand, may
#'   not report to work during legal holidays only.
#' @slot reg integer vector with length of 12
#'
#'   This represents the number of regular hours budgeted per month.
#' @slot leaveHours integer value defining the number of leave hours the
#'   employee can enjoy
#'
#'   This equates to paid vacation or sick leave.
#' @slot dcc character string defining the cost center wherein the excess
#'  regular man hours of the employee will be charged
#' @slot forecast logical value \cr
#'   Is the computation used for forecasting the present year?
#' @slot field logical value \cr
#'   Is the employee always on the field?
#' @slot dependents integer vector with length of 12
#'
#'   This represents the number of dependents of the employee per month.
#' @examples employee()
#' @export employee
employee <- setClass(
  "Employee",
  slots = c(ID = "character",
            name = "character",
            designation = "character",
            attendance = "numeric",
            spareFactor = "numeric",
            costCenter = "character",
            status = "character",
            cBegin = "character",
            cEnd = "character",
            inHouse = "logical",
            restday = "character",
            holHours = "integer",
            maxReg = "integer",
            reg = "integer",
            leaveHours = "integer",
            dcc = "character",
            forecast = "logical",
            field = "logical",
            dependents = "integer"),
  prototype = list(reg = rep(0L, times = 12),
                   dependents = rep(0L, times = 12),
                   attendance = rep(1, times = 12))
)

#' An S4 class representing a staff
#'
#' These are employees that are classified as
#'   \code{\link{DivisionManager-class}},
#'   \code{\link{GroupManager-class}},
#'   \code{\link{DepartmentManager-class}}
#'   \code{\link{SectionHead-class}}. In TMC, department managers and up are
#'   termed "Senior Staff" while section heads are termed "Junior Staff".
#'
#' @seealso \code{\link{Employee-class}}
#' @examples staff()
#' @export staff
staff <- setClass(
  "Staff",
  slots = character(0),
  contains = "Employee"
)

#' An S4 class representing a division manager
#'
#' Only one division manager (Mine DivisionManager) is presently employed in
#'   TMC Mines Group.
#'
#' @seealso \code{\link{Staff-class}}
#' @examples division_manager()
#' @export division_manager
division_manager <- setClass(
  "DivisionManager",
  slots = character(0),
  contains = "Staff"
)

#' An S4 class representing a group manager
#'
#' No group manager is presently employed in TMC Mines Group. The ex-Mines
#'   GroupManager was promoted to Mine DivisionManager.
#'
#' @seealso \code{\link{Staff-class}}
#' @examples group_manager()
#' @export group_manager
group_manager <- setClass(
  "GroupManager",
  slots = character(0),
  contains = "Staff"
)

#' An S4 class representing a department manager
#'
#' There are two departments existing at Mines Group: Mine Engineering
#'   Department and Mine Operations Department.
#'
#' @seealso \code{\link{Staff-class}}
#' @examples department_manager()
#' @export department_manager
department_manager <- setClass(
  "DepartmentManager",
  slots = character(0),
  contains = "Staff"
)

#' An S4 class representing a section head
#'
#' Foremen and department officers-in-charge are classified as section head.
#'   Some of the sections in Mines Group are Materials Handling, Survey, Grade
#'   Control, Conveyor System, Shipment and Road Maintenance.
#'
#' @seealso \code{\link{Staff-class}}
#' @examples section_head()
#' @export section_head
section_head <- setClass(
  "SectionHead",
  slots = character(0),
  contains = "Staff"
)

#' An S4 class representing an employee who is not a staff
#'
#' This class is entitled to premium pay for overtime work.
#'   \code{NonStaff-class} is further classified into \code{\link{Clerk-class}}
#'   and \code{\link{OperationPersonnel-class}}.
#'
#' @slot isRF logical value \cr
#'   Is the employee rank and file?
#' @slot regOT integer vector with length of 12
#'
#'   This represents the number of regular overtime hours budgeted per month.
#' @seealso \code{\link{Employee-class}}
#' @examples non_staff()
#' @export non_staff
non_staff <- setClass(
  "NonStaff",
  slots = c(isRF = "logical",
            regOT = "integer"),
  prototype = list(regOT = rep(0L, times = 12)),
  contains = "Employee"
)

#' An S4 class representing a clerk
#'
#' Clerks are mainly responsible for data encoding. Prior to the creation of
#'  \code{\link{mansched}}, they were responsible for man hours encoding
#'  using the spreadsheet template from accounting.
#'
#' @seealso \code{\link{NonStaff-class}}
#' @examples clerk()
#' @export clerk
clerk <- setClass(
  "Clerk",
  slots = character(0),
  contains = "NonStaff"
)

#' An S4 class representing an operation personnel
#'
#' Operation-related personnel are classified under this class. These personnel
#' have different scheduled rest days and thus have a different method of
#' calculating premium for overtime work compared to \code{\link{Clerk-class}}.
#' This class is further classified into \code{\link{Technical-class}} and
#' \code{\link{ProductionPersonnel-class}}.
#'
#' @slot rd integer vector with length of 12
#'
#'   This represents budgeted working hours of not more than 8 hours per day
#'   rendered on a scheduled rest day. Each element of the vector represents a
#'   month.
#' @slot rdOT integer vector with length of 12
#'
#'   This represents budgeted working hours of more than 8 hours per day
#'   rendered on a scheduled rest day. Each element of the vector represents a
#'   month.
#' @slot sh integer vector with length of 12
#'
#'   This represents budgeted working hours of not more than 8 hours per day
#'   rendered on a special holiday. Each element of the vector represents a
#'   month.
#'
#'   Only regular employees are entitled to a premium pay for this type of
#'   holiday.
#' @slot shOT integer vector with length of 12
#'
#'   This represents budgeted working hours of more than 8 hours per day
#'   rendered on a special holiday. Each element of the vector represents a
#'   month.
#'
#'   Only regular employees are entitled to a premium pay for this type of
#'   holiday.
#' @slot lh integer vector with length of 12
#'
#'   This represents budgeted working hours of not more than 8 hours per day
#'   rendered on a legal holiday. Each element of the vector represents a
#'   month.
#' @slot lhOT integer vector with length of 12
#'
#'   This represents budgeted working hours of more than 8 hours per day
#'   rendered on a legal holiday. Each element of the vector represents a
#'   month.
#' @slot nh integer vector with length of 12
#'
#'   This represents budgeted working hours of not more than 8 hours per day
#'   rendered on a negotiated holiday. Each element of the vector represents a
#'   month.
#'
#'   Only regular employees are entitled to a premium pay for this type of
#'   holiday.
#' @slot nhOT integer vector with length of 12
#'
#'   This represents budgeted working hours of more than 8 hours per day
#'   rendered on a negotiated holiday. Each element of the vector represents a
#'   month.
#'
#'   Only regular employees are entitled to a premium pay for this type of
#'   holiday.
#' @slot rs integer vector with length of 12
#'
#'   This represents budgeted working hours of not more than 8 hours per day
#'   rendered on a special holiday and a rest day. Each element of the vector
#'   represents a month.
#'
#'   Only regular employees are entitled to a compounded premium pay for this
#'   type of holiday.
#' @slot rsOT integer vector with length of 12
#'
#'   This represents budgeted working hours of more than 8 hours per day
#'   rendered on a special holiday and a rest day. Each element of the vector
#'   represents a month.
#'
#'   Only regular employees are entitled to a compounded premium pay for this
#'   type of holiday.
#' @slot rl integer vector with length of 12
#'
#'   This represents budgeted working hours of not more than 8 hours per day
#'   rendered on a legal holiday and a rest day. Each element of the vector
#'   represents a month.
#' @slot rlOT integer vector with length of 12
#'
#'   This represents budgeted working hours of more than 8 hours per day
#'   rendered on a legal holiday and a rest day. Each element of the vector
#'   represents a month.
#' @slot rn integer vector with length of 12
#'
#'   This represents budgeted working hours of not more than 8 hours per day
#'   rendered on a negotiated holiday and a rest day. Each element of the vector
#'   represents a month.
#'
#'   Only regular employees are entitled to a compounded premium pay for this
#'   type of holiday.
#' @slot rnOT integer vector with length of 12
#'
#'   This represents budgeted working hours of more than 8 hours per day
#'   rendered on a negotiated holiday and a rest day. Each element of the vector
#'   represents a month.
#'
#'   Only regular employees are entitled to a compounded premium pay for this
#'   type of holiday.
#' @seealso \code{\link{NonStaff-class}}
#' @examples operation_personnel()
#' @export operation_personnel
operation_personnel <- setClass(
  "OperationPersonnel",
  slots = c(lh = "integer",
            nh = "integer",
            rd = "integer",
            rl = "integer",
            rn = "integer",
            rs = "integer",
            sh = "integer",
            lhOT = "integer",
            nhOT = "integer",
            rdOT = "integer",
            rlOT = "integer",
            rnOT = "integer",
            rsOT = "integer",
            shOT = "integer"),
  prototype = list(lh = rep(0L, times = 12),
                   nh = rep(0L, times = 12),
                   rd = rep(0L, times = 12),
                   rl = rep(0L, times = 12),
                   rn = rep(0L, times = 12),
                   rs = rep(0L, times = 12),
                   sh = rep(0L, times = 12),
                   lhOT = rep(0L, times = 12),
                   nhOT = rep(0L, times = 12),
                   rdOT = rep(0L, times = 12),
                   rlOT = rep(0L, times = 12),
                   rnOT = rep(0L, times = 12),
                   rsOT = rep(0L, times = 12),
                   shOT = rep(0L, times = 12)),
  contains = "NonStaff"
)

#' An S4 class representing a technical personnel
#'
#' Engineers, some draftsmen and assistants providing technical support are
#'   classified under this S4 class. These personnel report on duty during the
#'   day only.
#'
#' @seealso \code{\link{OperationPersonnel-class}}
#' @examples technical()
#' @export technical
technical <- setClass(
  "Technical",
  slots = character(0),
  contains = "OperationPersonnel"
)

#' An S4 class representing a production personnel
#'
#' Production-related personnel are classified under this class. They may be
#'   assigned during night shift, thus, they are entitled for a night premium
#'   pay. This class is further classified into \code{\link{Supervisor-class}},
#'   \code{\link{Laborer-class}} and \code{\link{Operator-class}}.
#'
#' @seealso \code{\link{OperationPersonnel-class}}
#' @examples production_personnel()
#' @export production_personnel
production_personnel <- setClass(
  "ProductionPersonnel",
  slots = character(0),
  contains = "OperationPersonnel"
)

#' An S4 class representing a supervisor
#'
#' This class may include field engineers and lead-men. Supervisors report to
#'   their respective foremen or shift-in-charge. They are responsible for
#'   monitoring their \code{\link{Laborer-class}} and
#'   \code{\link{Operator-class}} personnel.
#'
#' @seealso \code{\link{ProductionPersonnel-class}}
#' @examples supervisor()
#' @export supervisor
supervisor <- setClass(
  "Supervisor",
  slots = character(0),
  contains = "ProductionPersonnel"
)

#' An S4 class representing a laborer
#'
#' Samplers, checkers, spotters, plumbers and \emph{etc.} are classified under
#'   this class.
#'
#' @seealso \code{\link{ProductionPersonnel-class}}
#' @examples laborer()
#' @export laborer
laborer <- setClass(
  "Laborer",
  slots = character(0),
  contains = "ProductionPersonnel"
)

#' An S4 class representing an operator
#'
#' This class includes authorized heavy and light equipment operators and
#'   drivers for trucks and services vehicles.
#'
#' @slot equipment character vector containing equipment types which the
#'   employee is authorized to operate
#' @seealso \code{\link{ProductionPersonnel-class}}
#' @examples operator()
#' @export operator
operator <- setClass(
  "Operator",
  slots = c(equipment = "character"),
  contains = "ProductionPersonnel"
)
