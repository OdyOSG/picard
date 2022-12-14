% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnostics.R
\name{get_diagnostics}
\alias{get_diagnostics}
\title{Function that gets the cohort diagnostics and prepares it for the viewer}
\usage{
get_diagnostics(execution_settings, cohort_meta, output_path)
}
\arguments{
\item{execution_settings}{executionSettings object with connection credentials}

\item{cohort_meta}{the cohort meta dataframe produced from build_boris_cohorts.
You can find this in the folder output/cohort_build/cohort_meta.rds. Use readr::read_rds to load.}

\item{output_path}{the path to output the results of the cohort diagnostics run}
}
\description{
Once the cohorts are created, we need to check on them using cohort diagnostics
This function will run cohort diagnostics and save the output to the designated
folder.
}
