#' Update input folder
#'
#' @param input_path the path to the input json files
#' @export
update_input_folder <- function(input_path = "input") {
  
  
  old_files <- system.file("boris_study/input",
                           package = "borisBayerStudy")
  
  
  dd <- file.copy(from = old_files,
                  to = here::here(),
                  overwrite = TRUE,
                  recursive = TRUE)
  
  usethis::ui_done("Updated Input Folder of Project")
  
}


verbose_build <- function(conn, sql, cohort_id, cohort_name, cohort_file) {
  
  usethis::ui_info("Generating Cohort Definition {ui_value(cohort_id)} {ui_value(cohort_name)}
                   using file {ui_field(cohort_file)}")
  DatabaseConnector::executeSql(conn, sql = sql)
  time <- Sys.time()
  return(time)
}

prep_cohort_sql <- function(cohort_sql,
                            cohort_id,
                            cdm_schema,
                            write_schema,
                            cohort_table,
                            dialect) {
  
  
  sql <- SqlRender::render(sql = cohort_sql,
                           cdm_database_schema = cdm_schema,
                           vocabulary_database_schema = cdm_schema,
                           target_database_schema = write_schema,
                           results_database_schema = write_schema,
                           target_cohort_table = cohort_table,
                           target_cohort_id = cohort_id,
                           results_database_schema.cohort_inclusion = paste(write_schema, paste0(cohort_table, "_inclusion"), sep="."),
                           results_database_schema.cohort_inclusion_result = paste(write_schema, paste0(cohort_table, "_inclusion_result"), sep="."),
                           results_database_schema.cohort_inclusion_stats = paste(write_schema, paste0(cohort_table, "_inclusion_stats"), sep="."),
                           results_database_schema.cohort_summary_stats = paste(write_schema, paste0(cohort_table, "_summary_stats"), sep="."),
                           results_database_schema.cohort_censor_stats = paste(write_schema, paste0(cohort_table, "_censor_stats"), sep="."),
                           warnOnMissingParameters = FALSE)
  
  sql <- SqlRender::translate(sql = sql,
                              targetDialect = dialect,
                              tempEmulationSchema = NULL)
  
}

get_cohort_counts <- function(execution_settings,
                              conn,
                              cohortId) {
  
  connectionDetails <- execution_settings$connectionDetails
  write_schema <- execution_settings$write_schema
  cohort_table <- execution_settings$cohort_table
  
  cohortTableFullName <- paste(write_schema, cohort_table, sep = ".")
  sql2 <- glue::glue("
  SELECT cohort_definition_id AS cohort_id,
    COUNT(*) AS cohort_entries,
    COUNT(DISTINCT subject_id) AS cohort_subjects
  FROM {cohortTableFullName}
  WHERE cohort_definition_id = {cohortId}
  GROUP BY cohort_definition_id;")
  
  sql2 <- SqlRender::translate(sql = sql2,targetDialect = connectionDetails$dbms)
  cnt <- DatabaseConnector::dbGetQuery(conn, sql2)
  df <- data.frame(
    cohort_entries = max(cnt$cohort_entries, 0),
    cohort_subjects = max(cnt$cohort_subjects, 0)
  )
  return(df)
  
}