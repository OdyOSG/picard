
#' Cohort builder
#'
#' This function builds all TREADS drug cohorts in a datbase
#'
#' @param execution_settings executionSettings object with connection credentials
#' @param input_path the path to the input json files
#' @param output_path the path to output the cohort build
#' @import magrittr
#' @include helpers.R
#' @export
build_cohorts <- function(execution_settings,
                                input_path,
                                output_path) {

  #create cohort table names
  name <- execution_settings$cohort_table
  cohortTableNames <- list(cohortTable = name,
                           cohortInclusionTable = paste0(name, "_inclusion"),
                           cohortInclusionResultTable = paste0(name, "_inclusion_result"),
                           cohortInclusionStatsTable = paste0(name, "_inclusion_stats"),
                           cohortSummaryStatsTable = paste0(name, "_summary_stats"),
                           cohortCensorStatsTable = paste0(name, "_censor_stats"))

  #cohorts to create table
  cohort_files <- list.files(input_path, recursive = TRUE, full.names = TRUE)
  cohort_names <- list.files(input_path, recursive = TRUE) %>%
    tools::file_path_sans_ext() %>%
    gsub("/", "_", .)

  cohortsToCreate <- tibble::tibble(
    cohortName = tools::file_path_sans_ext(basename(cohort_files)),
    json = purrr::map_chr(cohort_files, ~readr::read_file(.x))
  ) %>%
    dplyr::mutate(
      cohortId = dplyr::row_number()
    ) %>%
    dplyr::select(cohortId, cohortName, json)
  cohortsToCreate$sql <- purrr::map_chr(cohortsToCreate$json,
                                        ~CirceR::buildCohortQuery(CirceR::cohortExpressionFromJson(.x),
                                                                  CirceR::createGenerateOptions(generateStats = TRUE)))
  #generate cohorts using cohort generator
  cli::cat_rule(center = crayon::bgMagenta("Generating Cohorts for Study"))
  CohortGenerator::generateCohortSet(connectionDetails = execution_settings$connectionDetails,
                                     cdmDatabaseSchema = execution_settings$cdm_schema,
                                     cohortDatabaseSchema = execution_settings$write_schema,
                                     cohortTableNames = cohortTableNames,
                                     cohortDefinitionSet = cohortsToCreate,
                                     incremental = TRUE,
                                     incrementalFolder = file.path(output_path, execution_settings$databaseId))

  cohortCounts <- CohortGenerator::getCohortCounts(connectionDetails = execution_settings$connectionDetails,
                                                   cohortDatabaseSchema = execution_settings$write_schema,
                                                   cohortTable = execution_settings$cohort_table,
                                                   cohortDefinitionSet = cohortsToCreate) %>%
    dplyr::right_join(cohortsToCreate, by = c("cohortId", "cohortName", "json", "sql")) %>%
    tibble::as_tibble() %>%
    tidyr::replace_na(list(cohortEntries = 0, cohortSubjects = 0)) %>%
    dplyr::arrange(cohortId) %>%
    dplyr::mutate(database = execution_settings$databaseId)


  cohort_meta <- tibble::tibble(
    cohort_id = cohortCounts$cohortId,
    database_id = cohortCounts$database,
    study_name = execution_settings$studyName,
    cohort_name_short = cohortCounts$cohortName,
    cohort_name = cohort_names,
    cohort_file = cohort_files,
    cohort_json = cohortCounts$json,
    cohort_sql = cohortCounts$sql,
    cohort_entries = cohortCounts$cohortEntries,
    cohort_subjects = cohortCounts$cohortSubjects
  )


  readr::write_rds(cohort_meta, file = file.path(output_path, "cohort_meta.rds"))

  return(cohort_meta)
}



