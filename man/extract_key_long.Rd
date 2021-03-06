% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{extract_key_long}
\alias{extract_key_long}
\title{Read in inclusion in long format or variable list (in this case key.var is
variable)}
\usage{
extract_key_long(research.folder, database, file, key.var)
}
\arguments{
\item{research.folder}{The name of research folder. See
\code{\link{initResearchFolder}} for details on a research folder.}

\item{database}{Name of database. Should be either \code{private} or
\code{public} for flat tables, indicating whether data is stored in
\code{public_data} or \code{research/[research folder]/private_data}, or
the actual name of database if extracting data from a database.}

\item{file}{The inclusion or varible list.}

\item{key.var}{The name of variable based on which inclusion criterion is
specified. Vector with multiple elements is not allowed. To have complex
inclusion criteria based on multiple variables, generate one request form
for each variable.}
}
\description{
Read in inclusion in long format or variable list (in this case key.var is
variable)
}
