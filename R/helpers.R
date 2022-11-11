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

## Covariate Helpers -----------------------------------------------------

icd_chapters <- function() {

  icd <- tibble::tribble(
    ~conceptId, ~groupName, ~chapterName,
    0L, "Other Condition", "Other Condition",
    440371L, "Disorder of Immune Function", "Blood Disease",
    443723L, "Disorder of Cellular Component of Blood", "Blood Disease",
    432795L, "Traumatic AND/OR non-traumatic injury", "Injury and Poisoning",
    442562L, "Poisoning", "Injury and Poisoning",
    444363L, "Drug-related disorder", "Injury and Poisoning",
    440508L, "Congenital disease", "Congenital disease",
    435875L, "Complication of pregnancy, childbirth and/or the puerperium", "Pregnancy or childbirth disease",
    4088927L, "Pregnancy, childbirth and puerperium finding", 'Pregnancy or childbirth disease',
    4154314L, "Finding of arrangement of fetus" ,'Pregnancy or childbirth disease',
    4136529L, "Fetal movement finding", 'Pregnancy or childbirth disease',
    441406L, "Disorder of fetus or newborn", 'Perinatal disease',
    432250L, "Disorder due to infection", "Infection",
    438112L, "Neoplastic disease", "Neoplasm",
    31821L, "Disorder of endocrine system", 'Endocrine or metabolic disease',
    4090739L, "Nutritional disorder", 'Endocrine or metabolic disease',
    436670L, "Metabolic disease", 'Endocrine or metabolic disease',
    432586L, "Mental disorder", "Mental Disease",
    376337L, "Disorder of nervous system", "Nerve Disease and Pain",
    4011630L, "Neurological finding", "Nerve Disease and Pain",
    4038502L, "Eye / vision finding", "Eye Disease",
    4042836L, "Disorder of head", "ENT Disease",
    134057L, "Disorder of cardiovascular system", "Cardiovascular Disease",
    320136L, "Disorder of respiratory system", "Respiratory Disease",
    4302537L, "Digestive system finding" ,"Digestive Disease",
    4028387L, "Disorder of integument", "Skin Disease",
    4244662L, "Disorder of musculoskeletal system", "Soft tissue or bone disease",
    433595L, "Edema" ,"Soft tissue or bone disease",
    4344497L, "Soft tissue lesion", "Soft tissue or bone disease",
    40482430L, "Deformity of limb", "Soft tissue or bone disease",
    4027384L, "Inflammatory disorder", "Soft tissue or bone disease",
    4041285L, "Urogenital finding", "Genitourinary disease",
    4105886L, "Adverse reaction", "Iatrogenic condition",
    4053838L, "Foreign body", "Iatrogenic condition")

  return(icd)
}



rollupConditions <- function(connectionDetails,
                             cdmDatabaseSchema,
                             conceptIds) {

  #connect to database
  suppressMessages(connection <- DatabaseConnector::connect(connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))

  #change this to system file when turned into package
  conditionSql <- "with disease as ( -- define disease categories similar to ICD10 Chapters
  select 1 as precedence, 'Blood disease' as category_name, 440371 as category_id union
  select 1, 'Blood disease', 443723 union
  select 2, 'Injury and poisoning', 432795 union
  select 2, 'Injury and poisoning', 442562 union
  select 2, 'Injury and poisoning', 444363 union
  select 3, 'Congenital disease', 440508 union
  select 4, 'Pregnancy or childbirth disease', 435875 union
  select 4, 'Pregnancy or childbirth disease', 4088927 union
  select 4, 'Pregnancy or childbirth disease', 4154314 union
  select 4, 'Pregnancy or childbirth disease', 4136529 union
  select 5, 'Perinatal disease', 441406 union
  select 6, 'Infection', 432250 union
  select 7, 'Neoplasm', 438112 union
  select 8, 'Endocrine or metabolic disease', 31821 union
  select 8, 'Endocrine or metabolic disease', 4090739 union
  select 8, 'Endocrine or metabolic disease', 436670 union
  select 9, 'Mental disease', 432586 union
  select 10, 'Nerve disease and pain', 376337 union
  select 10, 'Nerve disease and pain', 4011630 union
  select 11, 'Eye disease', 4038502 union
  select 12, 'ENT disease', 4042836 union
  select 13, 'Cardiovascular disease', 134057 union
  select 14, 'Respiratory disease', 320136 union
  select 15, 'Digestive disease', 4302537 union
  select 16, 'Skin disease', 4028387 union
  select 17, 'Soft tissue or bone disease', 4244662 union
  select 17, 'Soft tissue or bone disease', 433595 union
  select 17, 'Soft tissue or bone disease', 4344497 union
  select 17, 'Soft tissue or bone disease', 40482430 union
  select 17, 'Soft tissue or bone disease', 4027384 union
  select 18, 'Genitourinary disease', 4041285 union
  select 19, 'Iatrogenic condition', 4105886 union
  select 19, 'Iatrogenic condition', 4053838
)
select distinct -- get the disease category with the lowest (best fitting) precedence, or assign 'Other Condition'
  concept_id as condition_id, concept_name as condition_name,
  first_value(coalesce(category_id, 0)) over (partition by concept_id order by precedence nulls last) as category_id,
  first_value(coalesce(category_name, 'Other Condition')) over (partition by concept_id order by precedence nulls last) as category_name
from @cdmDatabaseSchema.concept
left join ( -- find the approprate disease category, if possible
  select descendant_concept_id, category_id, category_name, precedence
  from @cdmDatabaseSchema.concept_ancestor
  join disease on ancestor_concept_id=category_id
) d on descendant_concept_id=concept_id
where concept_id in (@conceptIds) -- place here the concept_ids you want to roll up (have to be standard SNOMED)
;"
# conditionSql <- readr::read_file(system.file("sql/conditionRollup.sql", package = "ariadne"))
# #conditionSql <- readr::read_file("~/R/ideas/characterizationTables/sql/conditionRollup.sql")

conditionRollup <- DatabaseConnector::renderTranslateQuerySql(
  connection = connection,
  sql = conditionSql,
  snakeCaseToCamelCase = TRUE,
  cdmDatabaseSchema = cdmDatabaseSchema,
  conceptIds = conceptIds
)
names(conditionRollup) <- c("conceptId", "conceptName", "categoryId", "categoryName")
return(conditionRollup)

}


rollupDrugs <- function(connectionDetails,
                        cdmDatabaseSchema,
                        conceptIds) {

  #connect to database
  suppressMessages(connection <- DatabaseConnector::connect(connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))

  #change this to system file when turned into package
  drugSql <-"with drug as ( -- define disease categories similar to ICD10 Chapters
  select row_number() over (order by concept_code) as precedence, concept_name as category_name, concept_id as category_id
  from @cdmDatabaseSchema.concept
  where vocabulary_id='ATC' and concept_class_id='ATC 2nd'
)
select distinct -- get the disease category with the lowest (best fitting) precedence, or assign 'Other Condition'
  concept_id as drug_id, concept_name as drug_name,
  first_value(coalesce(category_id, 0)) over (partition by concept_id order by precedence nulls last) as category_id,
  first_value(coalesce(category_name, 'Other Drug')) over (partition by concept_id order by precedence nulls last) as category_name
from @cdmDatabaseSchema.concept
left join ( -- find the approprate drug category, if possible
  select descendant_concept_id, category_id, category_name, precedence
  from @cdmDatabaseSchema.concept_ancestor
  join drug on ancestor_concept_id=category_id
) d on descendant_concept_id=concept_id
where concept_id in (@conceptIds) -- place here the concept_ids you want to roll up (have to be standard SNOMED)
;"
#drugSql <- readr::read_file("~/R/ideas/characterizationTables/sql/drugRollup.sql")
#drugSql <- readr::read_file(system.file("sql/drugRollup.sql", package = "ariadne"))

drugRollup <- DatabaseConnector::renderTranslateQuerySql(
  connection = connection,
  sql = drugSql,
  snakeCaseToCamelCase = TRUE,
  cdmDatabaseSchema = cdmDatabaseSchema,
  conceptIds = conceptIds
) %>%
  mutate(categoryName = stringr::str_to_title(categoryName))

names(drugRollup) <- c("conceptId", "conceptName", "categoryId", "categoryName")

return(drugRollup)

}




summarize_strata <- function(src_df,
                             execution_settings,
                             strata_table,
                             strata_name) {

  schema_table_name <- paste(execution_settings$write_schema, strata_table, sep = ".")

  #summarize the strata for a meta file
  res <- src_df %>%
    dplyr::group_by(strata_id, cohort_definition_id) %>%
    dplyr::summarize(nn = sum(as.double(strata), na.rm = TRUE),
                     tot = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(pct_1 = nn / tot,
                  pct_0 = 1 - pct_1,
                  strata_name = strata_name,
                  strata_table = schema_table_name) %>%
    dplyr::select(strata_id, strata_name, strata_table,
                  cohort_definition_id, nn, pct_1, pct_0) %>%
    dplyr::collect()

  return(res)
}

get_driver <- function(execution_settings) {
  #determine the correct driver either postgres or redshift
  driver <- execution_settings$connectionDetails$dbms
  drv <- switch(driver,
                postgresql = RPostgres::Postgres,
                redshift = RPostgres::Redshift)
  return(drv)
}


append_to_strata_table <- function(con,
                                   execution_settings,
                                   strata_table,
                                   src_df) {
  #get strata id
  strata_id <- src_df %>%
    select(strata_id) %>%
    distinct() %>%
    collect() %>%
    as.integer()

  #identify the schemas
  cohort_schema <- execution_settings$write_schema
  schema_table_name <- paste(execution_settings$write_schema, strata_table, sep = ".")

  #insert into strata table
  ParallelLogger::logInfo("Append strata_id ", crayon::blue(strata_id),
                          " to ", crayon::green(schema_table_name))
  tik <- Sys.time()
  jj <- dplyr::rows_append(
    dplyr::tbl(con, dbplyr::in_schema(cohort_schema, strata_table)),
    src_df,
    in_place = TRUE
  )

  invisible(jj)
}

get_cohort <- function(con,
                       execution_settings,
                       cohort_id) {

  cohort_schema <- execution_settings$write_schema
  cohort_table <- execution_settings$cohort_table


  target_cohort_table <- dplyr::tbl(con,
                                    dbplyr::in_schema(cohort_schema, cohort_table)) %>%
    filter(cohort_definition_id == cohort_id)
  return(target_cohort_table)
}