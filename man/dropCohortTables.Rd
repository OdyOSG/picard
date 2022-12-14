% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeSchemaSetup.R
\name{dropCohortTables}
\alias{dropCohortTables}
\title{Drop cohort tables to refresh analysis for testing}
\usage{
dropCohortTables(configBlock, type = c("analysis", "diagnostics"))
}
\arguments{
\item{configBlock}{the header of the configuration block that needs to be
set as the active configuration}

\item{type}{the type of cohort tables to use either the tables for the analysis
or the tables for cohort diagnostics}
}
\description{
Drop cohort tables to refresh analysis for testing
}
