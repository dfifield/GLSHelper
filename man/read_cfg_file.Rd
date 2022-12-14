% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{read_cfg_file}
\alias{read_cfg_file}
\title{Read configuration file}
\usage{
read_cfg_file(cfgfile)
}
\arguments{
\item{cfgfile}{[character]\cr Required. The full pathname of a configuration
file. If ends in \code{.xlsx} or \code{.xls} it is assumed to be an \code{Excel} file (this is the
preferred format), otherwise it is assumed to be a text file.
This file contains one row per GLS data set to
be processed.}
}
\value{
A dataframe containing the configuration info.
}
\description{
Read a GLSHelper configuration file. Used
internally and not normally called by the user.
}
\details{
This function is normally called internally by
`\link{do_multi_geolocation} to read its configuration info. However, it can
be useful for the end-user during debugging of config file
formatting and reading.

See the \emph{Configuration file format} section of
\link{do_multi_geolocation} for info on column names, data types, and meanings.

An example configuration  file is included with this package
and can be found at the location given by the following command:

\code{system.file("extdata", "geolocation_settings.xlsx", package = "GLSHelper")}

Note, if you edit a CSV config file with \code{Excel} bad things may happen to
the formatting of dates and times.
}
\author{
Dave Fifield
}
