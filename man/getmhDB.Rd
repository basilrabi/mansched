% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mhDB_fun_getmhDB.R
\name{getmhDB}
\alias{getmhDB}
\title{Compute man hours database}
\usage{
getmhDB(empReq, empPool, sched, year = NA, hol = NA, forecast = FALSE)
}
\arguments{
\item{empReq}{a \code{\link{data.frame}} similar to \code{empReq} in
\code{\link{initEmpReq}}}

\item{empPool}{a \code{data.frame} similar to \code{empPool} in
\code{\link{initEmpPool}}}

\item{sched}{a \code{data.frame} similar to \code{sched} in
\code{\link{initEmpReq}}}

\item{year}{an integer value representing the year to be budgeted}

\item{hol}{a \code{data.frame} similar to \code{\link{holidays}}}

\item{forecast}{logical value \cr
Compute cost for forecast?}
}
\value{
a list containing the following:
  \enumerate{
    \item \code{data.frame} representing the man hours database of the
      assigned employees

      This is composed of the following columns:
      \describe{
        \item{ID}{character string representing the unique identifier of the
          real employee}
        \item{reqID}{character string representing the unique identifier of the
          real employee}
        \item{mh}{integer value representing the man hours assigned}
        \item{mhType}{character string representing the man hours type
          assigned (see \code{\link{assignEmp}})}
        \item{month}{integer value representing the month}
        \item{np}{integer value representing the man hours with night premium
          pay}
        \item{costCenter}{character string representing the cost center
          wherein the man hours is charged}
        \item{equipment}{character string of the equipment type should the
          required personnel is an operator}
      }
    \item list of \code{\link{Employee-class}} objects representing the
      theoretical employees with un-assigned man hours
    \item list of \code{\link{Employee-class}} objects representing the
      real employees with un-assigned man hours
    \item list of \code{\link{Employee-class}} objects representing the
      theoretical employees with assigned man hours
    \item list of \code{\link{Employee-class}} objects representing the
      real employees with assigned man hours
    \item \code{\link{data.frame}} containing un-assigned man hours of the
      theoretical employees

      This is composed of the following columns:
      \describe{
        \item{month}{integer value representing the month}
        \item{ID}{character string representing the employee requirement}
        \item{mhType}{man hour type}
        \item{mh}{integer value representing the unassigned man hours}
      }
    \item \code{\link{data.frame}} containing un-assigned man hours of the
      real employees

      This is composed of the following columns:
      \describe{
        \item{month}{integer value representing the month}
        \item{ID}{character string representing the employee requirement}
        \item{mhType}{man hour type}
        \item{mh}{integer value representing the unassigned man hours}
      }
    \item \code{data.frame} representing the man hours database of the
      employees with discarded OT hours

      This is composed of the following columns:
      \describe{
        \item{ID}{character string representing the unique identifier of the
          real employee}
        \item{reqID}{character string representing the unique identifier of the
          real employee}
        \item{mh}{integer value representing the man hours assigned}
        \item{mhType}{character string representing the man hours type
          assigned (see \code{\link{assignEmp}})}
        \item{month}{integer value representing the month}
        \item{np}{integer value representing the man hours with night premium
          pay}
        \item{costCenter}{character string representing the cost center
          wherein the man hours is charged}
      }
  }
}
\description{
Computes a man hours database throughout the year to be budgeted.
}
