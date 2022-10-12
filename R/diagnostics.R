#' Function that gets the cohort diagnostics and prepares it for the viewer
#'
#' Once the cohorts are created, we need to check on them using cohort diagnostics
#' This function will run cohort diagnostics and save the output to the designated
#' folder.
#'
#' @param execution_settings executionSettings object with connection credentials
#' @param cohort_meta the cohort meta dataframe produced from build_boris_cohorts.
#' You can find this in the folder output/cohort_build/cohort_meta.rds. Use readr::read_rds to load.
#' @param output_path the path to output the results of the cohort diagnostics run
#' @import usethis magrittr
#' @export
get_diagnostics <- function(execution_settings,
                            cohort_meta,
                            output_path) {

  #convert counts to dataframe
  cohortDataframe <- cohort_meta %>%
    dplyr::mutate(atlasId = cohort_id) %>%
    dplyr::select(atlasId, cohort_id, cohort_name, cohort_sql, cohort_json)
  names(cohortDataframe) <- c("atlasId", "cohortId", "cohortName", "sql", "json")

  #create cohort table names
  name <- execution_settings$cohort_table
  cohortTableNames <- list(cohortTable = name,
                           cohortInclusionTable = paste0(name, "_inclusion"),
                           cohortInclusionResultTable = paste0(name, "_inclusion_result"),
                           cohortInclusionStatsTable = paste0(name, "_inclusion_stats"),
                           cohortSummaryStatsTable = paste0(name, "_summary_stats"),
                           cohortCensorStatsTable = paste0(name, "_censor_stats"))


  databaseExportFolder <- file.path(output_path, execution_settings$databaseId)

  #run cohort Diagnostics
  CohortDiagnostics::executeDiagnostics(
    cohortDefinitionSet =  cohortDataframe,
    exportFolder = databaseExportFolder,
    cohortTableNames = cohortTableNames,
    cohortDatabaseSchema = execution_settings$write_schema,
    cdmDatabaseSchema = execution_settings$cdm_schema,
    vocabularyDatabaseSchema = execution_settings$vocabulary_schema,
    databaseId = execution_settings$databaseId,
    connectionDetails = execution_settings$connectionDetails,
    incremental = TRUE,
    minCellCount = 5
  )

  invisible(cohortDataframe)

}



# for_diagnostics <- function(input_path) {
#   ff <- list.files(input_path, recursive = TRUE) %>%
#     tibble::tibble(files = .) %>%
#     mutate(run_diagnostics = case_when(
#       grepl("covariates", files) ~ FALSE,
#       files == "outcome/alll_cause_death.json" ~ FALSE,
#       TRUE ~ TRUE
#     ))
#
#   return(ff)
# }
