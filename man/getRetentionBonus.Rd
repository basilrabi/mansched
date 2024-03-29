% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/employee_fun_getRetentionBonus.R
\name{getRetentionBonus}
\alias{getRetentionBonus}
\alias{getRetentionBonus,Employee-method}
\title{Compute retention bonus}
\usage{
getRetentionBonus(theObject)

\S4method{getRetentionBonus}{Employee}(theObject)
}
\arguments{
\item{theObject}{\code{\link{Employee-class}} object}
}
\value{
a \code{\link{data.frame}} with 12 rows and 3 columns.

  Each row represents a month. The columns are:
  \describe{
     \item{ID}{character string representing the unique identifier of the
       real employee}
     \item{month}{integer value representing the month}
     \item{retentionBonus}{numeric value defining the cost charged for that
       month}
  }
}
\description{
Retention bonus to be given to all seasonal employees at the end of their
  employment contract
}
\section{Functions}{
\itemize{
\item \code{getRetentionBonus(Employee)}: Compute retention bonus

}}
