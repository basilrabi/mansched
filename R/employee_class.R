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
#' @slot totHours integer vector with length of 12
#'
#'   This represents the number of hours per month which is equated to the
#'   employee's basic monthly wage.
#' @slot reg integer vector with length of 12
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
    totHours = 'integer',
    reg = 'integer'
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

#' An S4 class representing an employee who is not a staff
#'
#' This class is entitled to premium pay for overtime work.
#'   \code{Non Staff-class} is further classified into \code{\link{Clerk-class}}
#'   and \code{\link{Operation Personnel-class}}.
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
  'Non Staff',
  slots = c(
    isRF = 'logical',
    regOT = 'integer'
  ),
  contains = 'Employee'
)

#' An S4 class representing a clerk
#'
#' Clerks are mainly responsible for data encoding. Prior to the creation of
#'  \code{\link{mansched2}}, they were responsible for man hours encoding
#'  using the spreadsheet template from accounting.
#'
#' @seealso \code{\link{Non Staff-class}}
#' @examples clerk()
#' @export clerk
clerk <- setClass(
  'Clerk',
  slots = character(0),
  contains = 'Non Staff'
)

#' An S4 class representing an operation personnel
#'
#' Operation-related personnel are classified under this class. These personnel
#' have different scheduled rest days and thus have a different method of
#' calculating premium for overtime work compared to \code{\link{Clerk-class}}.
#' This class is further classified into \code{\link{Technical-class}} and
#' \code{\link{Production Personnel-class}}.
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
#' @seealso \code{\link{Non Staff-class}}
#' @examples operation_personnel()
#' @export operation_personnel
operation_personnel <- setClass(
  'Operation Personnel',
  slots = c(
    rd = 'integer',
    rdOT = 'integer',
    sh = 'integer',
    shOT = 'integer',
    lh = 'integer',
    lhOT = 'integer',
    nh = 'integer',
    nhOT = 'integer',
    rs = 'integer',
    rsOT = 'integer',
    rl = 'integer',
    rlOT = 'integer',
    rn = 'integer',
    rnOT = 'integer'
  ),
  contains = 'Non Staff'
)

#' An S4 class representing a technical personnel
#'
#' Engineers, some draftsmen and assistants providing technical support are
#'   classified under this S4 class. These personnel report on duty during the
#'   day only.
#'
#' @seealso \code{\link{Operation Personnel-class}}
#' @examples technical()
#' @export technical
technical <- setClass(
  'Technical',
  slots = character(0),
  contains = 'Operation Personnel'
)

#' An S4 class representing a production personnel
#'
#' Production-related personnel are classified under this class. They may be
#'   assigned during night shift, thus, they are entitled for a night premium
#'   pay. This class is further classified into \code{\link{Supervisor-class}},
#'   \code{\link{Laborer-class}} and \code{\link{Operator-class}}.
#'
#' @seealso \code{\link{Operation Personnel-class}}
#' @examples production_personnel()
#' @export production_personnel
production_personnel <- setClass(
  'Production Personnel',
  slots = character(0),
  contains = 'Operation Personnel'
)

#' An S4 class representing a supervisor
#'
#' This class may include field engineers and lead-men. Supervisors report to
#'   their respective foremen or shift-in-charge. They are responsible for
#'   monitoring their \code{\link{Laborer-class}} and
#'   \code{\link{Operator-class}} personnel.
#'
#' @seealso \code{\link{Production Personnel-class}}
#' @examples supervisor()
#' @export supervisor
supervisor <- setClass(
  'Supervisor',
  slots = character(0),
  contains = 'Production Personnel'
)

#' An S4 class representing a laborer
#'
#' Samplers, checkers, spotters, plumbers and \emph{etc.} are classified under
#'   this class.
#'
#' @seealso \code{\link{Production Personnel-class}}
#' @examples laborer()
#' @export laborer
laborer <- setClass(
  'Laborer',
  slots = character(0),
  contains = 'Production Personnel'
)

#' An S4 class representing an operator
#'
#' This class includes authorized heavy and light equipment operators and
#'   drivers for trucks and services vehicles.
#'
#' @slot equipment character vector containing equipment types which the
#'   employee is authorized to operate
#' @seealso \code{\link{Production Personnel-class}}
#' @examples operator()
#' @export operator
operator <- setClass(
  'Operator',
  slots = c(
    equipment = 'character'
  ),
  contains = 'Production Personnel'
)
