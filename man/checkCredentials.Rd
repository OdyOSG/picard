% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/configSetup.R
\name{checkCredentials}
\alias{checkCredentials}
\title{Check credentials}
\usage{
checkCredentials(
  configBlock,
  credentials = c("dbms", "user", "password", "server", "port", "cdm", "vocab", "write")
)
}
\arguments{
\item{configBlock}{the header of the configuration block that needs to be
set as the active configuration}

\item{credentials}{a character vector of credentials to check}
}
\description{
Check credentials
}
