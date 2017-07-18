#' @import methods
NULL

#' An S4 class defining an employee
#'
#' This class is based on the organizational structure ofTaganito Mining
#'   Corporation's (TMC) Mines Group. This can be used in representing
#'   both real and theoretical employees.
#'
#' Real employees are actual persons represented as \code{Employee-class}
#'   objects. Theoretical employees in this context are employee requirements
#'   represented as \code{Employee-class} objects.
#'
#'   This class is further classified into \code{\link{Staff-class}} and
#'   \code{\link{Non Staff-class}}.
#'
#' @slot ID character string defining the employee's unique identifier
#' @slot name character string defining the name of employee
#' @slot designation character string defining the designation of the
#'   employee
#' @slot attendance numeric value estimating the attendance rate of the
#'   employee
#'
#'   This defaults to 1 for theoretical employees. For real employees,
#'   a value of not more than 1 but greater than 0.5 may be used.
#' @slot spareFactor numeric value estimating the man power spare factor for
#'   an activity
#'
#'   This is usually used as man hours multiplier when a spare operator is
#'   assigned. The spare factor defaults to 1 for real employees. For
#'   theoretical employees, a value greater than 1 may be used.
#' @slot costCode character vector containing the cost codes wherein the
#'   employee will be charged
#' @slot status character string defining the employment status of the employee
#' @slot cBegin character string defining the date of start of employment of the
#'   employee
#' @slot cEnd character string defining the date of end of employment of the
#'   employee
#' @slot inHouse logical value \cr
#'   Is the employee's accomodation given by the company?
#' @slot isTheo logical value \cr
#'   Is the employee theoretical?
#' @slot restday character string defining the day of the week wherein the
#'   employee is not required to report to work
#' @slot holHours integer vector with length of 12
#'
#'   This represents the total number hours consisting of only the first eight
#'   hours of a holiday not scheduled on a rest day of employee. Each element
#'   of the vector represents a month.
#'
#'   Even if the employee will not report on duty, he or she will be paid by
#'   this number of hours.
#'   This slot only applies to real employees.
#' @slot regularTime integer vector with length of 12
#'
#'   This represents the number of regular hours budgeted per month.
#' @examples employee()
#' @export employee
employee <- setClass(
  'Employee',
  slots = c(
    ID = 'character',
    name = 'character',
    designation = 'character',
    attendance = 'numeric',
    spareFactor = 'numeric',
    costCode = 'character',
    status = 'character',
    cBegin = 'character',
    cEnd = 'character',
    inHouse = 'logical',
    isTheo = 'logical',
    restday = 'character',
    holHours = 'integer',
    # insert total payable reg hours per month
    regularTime = 'integer'
  )
)

#' An S4 class representing a staff
#'
#' These are employees that are classified as
#'   \code{\link{Division Manager-class}},
#'   \code{\link{Group Manager-class}},
#'   \code{\link{Department Manager-class}}
#'   \code{\link{Section Head-class}}. In TMC, department managers and up are
#'   termed 'Senior Staff' while section heads are termed 'Junior Staff'.
#'
#' @seealso \code{\link{Employee-class}}
#' @examples staff()
#' @export staff
staff <- setClass(
  'Staff',
  slots = character(0),
  contains = 'Employee'
)

#' An S4 class representing a division manager
#'
#' Only one division manager (Mine Division Manager) is presently employed in
#'   TMC Mines Group.
#'
#' @seealso \code{\link{Staff-class}}
#' @examples division_manager()
#' @export division_manager
division_manager <- setClass(
  'Division Manager',
  slots = character(0),
  contains = 'Staff'
)

#' An S4 class representing a group manager
#'
#' No group manager is presently employed in TMC Mines Group. The ex-Mines
#'   Group Manager was promoted to Mine Division Manager.
#'
#' @seealso \code{\link{Staff-class}}
#' @examples group_manager()
#' @export group_manager
group_manager <- setClass(
  'Group Manager',
  slots = character(0),
  contains = 'Staff'
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
  'Department Manager',
  slots = character(0),
  contains = 'Staff'
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
  'Section Head',
  slots = character(0),
  contains = 'Staff'
)
