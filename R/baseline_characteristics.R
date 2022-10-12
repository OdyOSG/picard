#cohort_meta <- readr::read_rds(here::here("output/cohort_build/cohort_meta.rds"))


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



baseline_demographics_cat <- function(execution_settings,
                                      target_cohort_id,
                                      strata = NULL,
                                      output_path) {

  #preset feature extraction for demographic categories
  cov_settings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                             useDemographicsAgeGroup = TRUE,
                                                             useDemographicsRace = TRUE,
                                                             useDemographicsIndexYear = TRUE)

  #extract data
  cov <- FeatureExtraction::getDbCovariateData(connectionDetails = execution_settings$connectionDetails,
                                               cdmDatabaseSchema = execution_settings$cdm_schema,
                                               cohortTable = execution_settings$cohort_table,
                                               cohortDatabaseSchema = execution_settings$write_schema,
                                               cohortId = target_cohort_id,
                                               covariateSettings = cov_settings)


  #get sqlite connection
  sqlite <- RSQLite::SQLite()
  con <- DBI::dbConnect(sqlite, cov@dbname)

  if (is.null(strata)) {
    strata <- dplyr::tbl(con, "covariates") %>%
      distinct(rowId) %>%
      mutate(strata = 1)
  } else {
    checkmate::assert_class(strata, "strata")
    strata <- dplyr::copy_to(con, strata$data, name = "strata", overwrite = TRUE)
  }

  #get the denominator for the strata counts
  denom <- strata %>%
    group_by(strata) %>%
    count(name = "tot") %>%
    dplyr::copy_to(con, ., name = "denom", overwrite = TRUE)


  #aggregate and formate data
  dem_cat_tbl <- dplyr::tbl(con, "covariates") %>%
    dplyr::left_join(strata, by = "rowId") %>%
    dplyr::mutate(conceptId = dbplyr::sql("CAST(SUBSTRING(covariateId, 0, LENGTH(covariateId) - 4) AS INT)"),
                  analysisId = dbplyr::sql("CAST(SUBSTRING(covariateId, LENGTH(covariateId) - 4, LENGTH(covariateId)) AS INT)")) %>%
    dplyr::group_by(analysisId, conceptId, strata) %>%
    dplyr::summarize(nn = sum(covariateValue, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(conceptName = dplyr::case_when(
      conceptId == 8507 ~ "Male",
      conceptId == 8532 ~ "Female",
      conceptId == 1 ~ "5 - 9",
      conceptId == 2 ~ "10 - 14",
      conceptId == 3 ~ "15 - 19",
      conceptId == 4 ~ "20 - 24",
      conceptId == 5 ~ "25 - 29",
      conceptId == 6 ~ "30 - 34",
      conceptId == 7 ~ "35 - 39",
      conceptId == 8 ~ "40 - 44",
      conceptId == 9 ~ "45 - 49",
      conceptId == 10 ~ "50 - 54",
      conceptId == 11 ~ "55 - 59",
      conceptId == 12 ~ "60 - 64",
      conceptId == 13 ~ "65 - 69",
      conceptId == 14 ~ "70 - 74",
      conceptId == 15 ~ "75 - 79",
      conceptId == 16 ~ "80 - 84",
      conceptId == 17 ~ "85 - 89",
      conceptId == 18 ~ "90 - 94",
      conceptId == 19 ~ "95 - 99",
      conceptId == 20 ~ "100 - 104",
      conceptId == 8657 ~ "American Indian or Alaska Native",
      conceptId == 8515 ~ "Asian",
      conceptId == 8516 ~ "Black or African American",
      conceptId == 8557 ~ "Native Hawaiian or Other Pacific Islander",
      conceptId == 8527 ~ "White",
      TRUE ~ as.character(conceptId)),
      groupName =  dplyr::case_when(
        analysisId == 1 ~ "Gender",
        analysisId == 3 ~ "Age Group",
        analysisId == 4 ~ "Race",
        analysisId == 6 ~ "Index Year",
        TRUE ~ as.character(analysisId)
      )) %>%
    dplyr::left_join(denom, by = c("strata")) %>%
    dplyr::mutate(pct_raw = nn / tot,
                  chapterName = "Categorical",
                  domain = "Demographics") %>%
    dplyr::select(domain, chapterName, groupName, conceptName,
                  conceptId, strata, nn, pct_raw) %>%
    dplyr::collect()

  #save output
  save_path <- file.path(output_path, "demographics_baseline")

  arrow::write_feather(dem_cat_tbl, sink = save_path)
  usethis::ui_info("Baseline demographic covariates written as a feather file to {ui_field(save_path)}")

  #exit
  invisible(dem_cat_tbl)
  DBI::dbDisconnect(con)
}



baseline_continuous <- function(execution_settings,
                                cohort_meta,
                                target_cohort_id,
                                strata = NULL,
                                output_path) {

  #preset feature extraction for demographic categories
  cov_settings <- FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE,
                                                             useDemographicsPriorObservationTime = TRUE,
                                                             useDemographicsPostObservationTime = TRUE,
                                                             useDemographicsTimeInCohort = TRUE,
                                                             useDcsi = TRUE)

  #extract data
  cov <- FeatureExtraction::getDbCovariateData(connectionDetails = execution_settings$connectionDetails,
                                               cdmDatabaseSchema = execution_settings$cdm_schema,
                                               cohortTable = execution_settings$cohort_table,
                                               cohortDatabaseSchema = execution_settings$write_schema,
                                               cohortId = target_cohort_id,
                                               covariateSettings = cov_settings)


  #get sqlite connection
  sqlite <- RSQLite::SQLite()
  con <- DBI::dbConnect(sqlite, cov@dbname)

  #add strata to analysis
  if (is.null(strata)) {
    strata <- dplyr::tbl(con, "covariates") %>%
      distinct(rowId) %>%
      mutate(strata = 1)
  } else {
    checkmate::assert_class(strata, "strata")
    strata <- dplyr::copy_to(con, strata$data, name = "strata", overwrite = TRUE)
  }

  #aggregate and formate data
  cts_tbl <-  dplyr::tbl(con, "covariates") %>%
    dplyr::left_join(dplyr::tbl(con, "covariateRef"), by = c("covariateId")) %>%
    dplyr::left_join(dplyr::tbl(con, "analysisRef"), by = c("analysisId")) %>%
    dplyr::left_join(strata, by = "rowId") %>%
    dplyr::collect() %>%
    group_by(analysisId, covariateId, strata) %>%
    summarize(min = min(covariateValue),
              median = median(covariateValue),
              iqr = IQR(covariateValue),
              max = max(covariateValue)) %>%
    ungroup() %>%
    dplyr::mutate(analysisName = case_when(
      analysisId == 2 ~ "Age",
      analysisId == 8 ~ "Prior Observation Time",
      analysisId == 9 ~ "Post Observation Time",
      analysisId == 10 ~ "Time In Cohort",
      analysisId == 902 ~ "DSCI",
      TRUE ~ as.character(analysisId)
    ),
    domain = "Continuous",
    chapterName = ifelse(analysisId == 902, "Condition", "Demographics"),
    groupName = analysisName,
    conceptName = analysisName,
    conceptId = covariateId
    ) %>%
    dplyr::select(domain, chapterName, groupName, conceptName, conceptId,
                  strata, min:max)


  #save output
  save_path <- file.path(output_path, "continuous_baseline")

  arrow::write_feather(cts_tbl , sink = save_path)
  usethis::ui_info("Baseline continuous covariates written as a feather file to {ui_field(save_path)}")

  #exit
  invisible(cts_tbl)
  DBI::dbDisconnect(con)
}

baseline_conditions <- function(execution_settings,
                                target_cohort_id,
                                strata = NULL,
                                output_path) {

  #preset feature extraction for demographic categories
  cov_settings <- FeatureExtraction::createCovariateSettings(useConditionGroupEraLongTerm = TRUE)

  #extract data
  cov <- FeatureExtraction::getDbCovariateData(connectionDetails = execution_settings$connectionDetails,
                                               cdmDatabaseSchema = execution_settings$cdm_schema,
                                               cohortTable = execution_settings$cohort_table,
                                               cohortDatabaseSchema = execution_settings$write_schema,
                                               cohortId = target_cohort_id,
                                               covariateSettings = cov_settings)

  #get sqlite connection
  sqlite <- RSQLite::SQLite()
  con <- DBI::dbConnect(sqlite, cov@dbname)

  #add strata to analysis
  if (is.null(strata)) {
    strata <- dplyr::tbl(con, "covariates") %>%
      distinct(rowId) %>%
      mutate(strata = 1)
  } else {
    checkmate::assert_class(strata, "strata")
    strata <- dplyr::copy_to(con, strata$data, name = "strata", overwrite = TRUE)
  }

  #get the denominator for the strata counts
  denom <- strata %>%
    group_by(strata) %>%
    count(name = "tot") %>%
    dplyr::copy_to(con, ., name = "denom", overwrite = TRUE)

  #collect all concept ids used
  icd_grp <- ariadne:::rollupConditions(
    connectionDetails = execution_settings$connectionDetails,
    cdmDatabaseSchema = execution_settings$cdm_schema,
    conceptIds = dplyr::tbl(con, "covariateRef") %>%
      dplyr::pull(conceptId) %>%
      as.integer()) %>%
    left_join(icd_chapters(), by = c("categoryId" = "conceptId")) %>%
    select(conceptId, conceptName, categoryId, groupName, chapterName) %>%
    dplyr::copy_to(con, ., name = "icd", overwrite = TRUE)



  #aggregate and formate data for raw format
  conditions_tbl <- dplyr::tbl(con, "covariates") %>%
    dplyr::left_join(strata, by = "rowId") %>%
    mutate(conceptId = sql("CAST(SUBSTRING(covariateId, 0, LENGTH(covariateId) - 4) AS INT)")) %>%
    group_by(conceptId, strata) %>%
    summarize(nn = sum(covariateValue, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(icd_grp, by = c("conceptId")) %>%
    mutate(domain = "Conditions Raw") %>%
    select(domain, chapterName, groupName, conceptName,
           conceptId, strata, nn) %>%
    dplyr::union(
      dplyr::tbl(con, "covariates") %>%
        dplyr::left_join(strata, by = "rowId") %>%
        dplyr::mutate(conceptId = sql("CAST(SUBSTRING(covariateId, 0, LENGTH(covariateId) - 4) AS INT)")) %>%
        dplyr::left_join(icd_grp, by = c("conceptId")) %>%
        dplyr::distinct(rowId, categoryId, groupName, chapterName, strata) %>%
        dplyr::group_by(categoryId, groupName, chapterName, strata) %>%
        dplyr::count(name = "nn") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(nn = as.double(nn),
                      domain = "Conditions Group",
                      conceptId = categoryId,
                      conceptName = groupName) %>%
        dplyr::select(domain, chapterName, groupName, conceptName,
                      conceptId, strata, nn)
    ) %>%
    dplyr::left_join(denom, by = c("strata")) %>%
    dplyr::mutate(pct_raw = nn / tot) %>%
    dplyr::select(domain, chapterName, groupName, conceptName,
                  conceptId, strata, nn, pct_raw) %>%
    dplyr::collect()

  #save output
  save_path <- file.path(output_path, "conditions_baseline")

  arrow::write_feather(conditions_tbl, sink = save_path)
  usethis::ui_info("Baseline condition covariates written as a feather file to {ui_field(save_path)}")

  invisible(conditions_tbl)
  DBI::dbDisconnect(con)
}


baseline_drugs <- function(execution_settings,
                           target_cohort_id,
                           strata = NULL,
                           output_path) {

  #preset feature extraction for demographic categories
  cov_settings <- FeatureExtraction::createCovariateSettings(useDrugGroupEraLongTerm = TRUE,
                                                             excludedCovariateConceptIds = c(
                                                               21600001, 21600959, 21601237,
                                                               21601907, 21602359, 21602681,
                                                               21602795, 21601386, 21603931,
                                                               21604180, 21604847, 21605007,
                                                               21603550, 21605212)) #remove ATC 1st class)

  #extract data
  cov <- FeatureExtraction::getDbCovariateData(connectionDetails = execution_settings$connectionDetails,
                                               cdmDatabaseSchema = execution_settings$cdm_schema,
                                               cohortTable = execution_settings$cohort_table,
                                               cohortDatabaseSchema = execution_settings$write_schema,
                                               cohortId = target_cohort_id,
                                               covariateSettings = cov_settings)

  #get sqlite connection
  sqlite <- RSQLite::SQLite()
  con <- DBI::dbConnect(sqlite, cov@dbname)

  #add strata to analysis
  if (is.null(strata)) {
    strata <- dplyr::tbl(con, "covariates") %>%
      distinct(rowId) %>%
      mutate(strata = 1)
  } else {
    checkmate::assert_class(strata, "strata")
    strata <- dplyr::copy_to(con, strata$data, name = "strata", overwrite = TRUE)
  }

  #get the denominator for the strata counts
  denom <- strata %>%
    group_by(strata) %>%
    count(name = "tot") %>%
    dplyr::copy_to(con, ., name = "denom", overwrite = TRUE)

  #collect all concept ids used
  atc_grp <- ariadne:::rollupDrugs(
    connectionDetails = execution_settings$connectionDetails,
    cdmDatabaseSchema = execution_settings$cdm_schema,
    conceptIds = dplyr::tbl(con, "covariateRef") %>%
      dplyr::pull(conceptId) %>%
      as.integer()) %>%
    dplyr::rename(groupName = categoryName) %>%
    dplyr::mutate(chapterName = groupName) %>%
    dplyr::copy_to(con, ., name = "atc", overwrite = TRUE)



  #aggregate and formate data
  drugs_tbl <- dplyr::tbl(con, "covariates") %>%
    dplyr::left_join(strata, by = "rowId") %>%
    mutate(conceptId = sql("CAST(SUBSTRING(covariateId, 0, LENGTH(covariateId) - 4) AS INT)")) %>%
    group_by(conceptId, strata) %>%
    summarize(nn = sum(covariateValue, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(atc_grp, by = c("conceptId")) %>%
    mutate(domain = "Drugs Raw") %>%
    left_join(denom, by = c("strata")) %>%
    mutate(pct_raw = nn / tot) %>%
    select(domain, chapterName, groupName,
           conceptName, conceptId,
           strata, nn, pct_raw) %>%
    dplyr::collect()

  save_path <- file.path(output_path, "drugs_baseline")

  arrow::write_feather(drugs_tbl, sink = save_path)
  usethis::ui_info("Baseline drug covariates written as a feather file to {ui_field(save_path)}")

  invisible(drugs_tbl)
  DBI::dbDisconnect(con)
}


baseline_cohorts <- function(execution_settings,
                             target_cohort_id,
                             covariate_cohort_ids,
                             covariate_cohort_names,
                             strata = NULL,
                             output_path) {


  #determine the correct driver either postgres or redshift
  driver <- execution_settings$connectionDetails$dbms
  drv <- switch(driver,
                postgresql = RPostgres::Postgres,
                redshift = RPostgres::Redshift)

  #connect to dbms
  con <- DBI::dbConnect(
    drv = drv(),
    host = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[1]],
    dbname = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[2]],
    user = execution_settings$connectionDetails$user(),
    password = execution_settings$connectionDetails$password(),
    port = execution_settings$connectionDetails$port()
  )


  cohort_schema <- execution_settings$write_schema
  cohort_table <- execution_settings$cohort_table

  cohort_key <- tibble::tibble(
    cohortId = covariate_cohort_ids,
    cohortName = covariate_cohort_names
  )

  target_cohort_table <- dplyr::tbl(con, dbplyr::in_schema(cohort_schema, cohort_table)) %>%
    filter(cohort_definition_id == target_cohort_id)

  #add strata to analysis
  if (is.null(strata)) {
    strata <- target_cohort_table %>%
      distinct(subject_id) %>%
      rename(rowId = subject_id) %>%
      mutate(strata = 1)
  } else {
    checkmate::assert_class(strata, "strata")
    strata <- dplyr::copy_to(con, strata$data, name = "strata", overwrite = TRUE)
  }

  #get the denominator for the strata counts
  denom <- strata %>%
    group_by(strata) %>%
    count(name = "tot") %>%
    dplyr::copy_to(con, ., name = "denom", overwrite = TRUE)


  covariate_cohort_table <- dplyr::tbl(con, dbplyr::in_schema(cohort_schema, cohort_table)) %>%
    filter(cohort_definition_id %in% covariate_cohort_ids)




  cohort_tbl <- target_cohort_table %>%
    dplyr::left_join(strata, by = c( "subject_id" = "rowId")) %>%
    dplyr::inner_join(covariate_cohort_table, by = c("subject_id"),
                      suffix = c("_target", "_covariate")) %>%
    dplyr::mutate(shift = cohort_start_date_target - lubridate::days(365),
                  hit = dplyr::if_else(shift <= cohort_start_date_covariate, 1L, 0L, 0L)) %>%
    dplyr::group_by(cohort_definition_id_covariate, strata) %>%
    dplyr::summarize(nn = sum(hit, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(nn = as.double(nn),
                  domain = "Cohorts",
                  chapterName = "Conditions",
                  groupName = "Conditions",
                  cohortId = cohort_definition_id_covariate) %>%
    dplyr::left_join(denom, by = c("strata")) %>%
    dplyr::select(-cohort_definition_id_covariate) %>%
    dplyr::mutate(pct_raw = nn / tot) %>%
    dplyr::arrange(cohortId, strata) %>%
    dplyr::collect() %>%
    dplyr::left_join(cohort_key, by = c("cohortId")) %>%
    dplyr::select(domain, chapterName, groupName, cohortName, cohortId,
                  strata, nn, pct_raw)


  save_path <- file.path(output_path, "cohorts_baseline")

  arrow::write_feather(cohort_tbl, sink = save_path)
  usethis::ui_info("Baseline cohort covariates written as a feather file to {ui_field(save_path)}")

  invisible(cohort_tbl)
  DBI::dbDisconnect(con)

}



#' Function that gets the baseline covariates for the analysis
#'
#' The baseline covariates are used to create the summary table 1 of our analysis. Further
#' we can assess baseline comorbidities for our population of interest. This function
#' creates a feather file for each set of baseline covariates: demographics, conditions, drugs, cohorts,
#' and continuous covariates. Conditions are extracted based on condition group eras with
#' a 365 day look back period. We also create condition summaries based on
#' icd chapters. The ICD chapter rollup is used for the table 1 in the written report. The drugs
#' are created based on a 365 day look back period and the features are based on drug era groups.
#' The cohort covariates are a set of pre-defined cohorts that have been loaded to the cohort table. See
#' the input/cohorts_to_create folder for the covariate cohorts. Continuous covariates are those that
#' must be summarized using min, max, median and IQR. This includes age, time of observation and DSCI.
#' Each type of baseline covariate is saved as a feather file in the output/baseline folder. We can
#' also include a strata for the baseline covariates. The saving convention is automated based on the
#' strata used.
#'
#' @param execution_settings executionSettings object with connection credentials
#' @param target_cohort_id an integer specifying the cohort id to use. Please review
#' the cohort_meta file in output/cohort_build to determine the cohort definition ids
#' @param cohort_meta file that specifies all cohorts created for the study.
#' Function will extract covariate cohorts only for the analysis.
#' @param strata a dateframe from a strata function
#' @param output_path the output path to save the file. Default output/baseline
#' @import magrittr
#' @export
generate_baseline_covariates <- function(execution_settings,
                                         target_cohort_id,
                                         cohort_meta,
                                         strata = NULL,
                                         output_path = "output/baseline") {

  #get the ids of the cohort definitions used as covariates
  df <- cohort_meta %>%
    filter(grepl("covariates", cohort_file) | grepl("strata", cohort_file)) %>%
    select(cohort_id, cohort_name) %>%
    mutate(cohort_name = gsub("covariates_", "", cohort_name),
           cohort_name = gsub("strata_", "", cohort_name))

  cohort_name <- cohort_meta %>%
    dplyr::filter(cohort_id == target_cohort_id) %>%
    pull(cohort_name)

  #get database in use
  database <- execution_settings$databaseId

  #edit path to strata name
  if (is.null(strata)) {
    output_path <- file.path(output_path, database, cohort_name, "strata_total")
  } else{

    strata_dir <- dplyr::case_when(
      strata$strata_name == "age_65" ~ "age65",
      strata$strata_name == "gender_8532" ~ "female",
      strata$strata_name == "date_202003" ~ "covid",
      strata$strata_name == "cohort_17" ~ "t2d"
    ) %>%
      paste("strata", ., sep = "_")
    output_path <- file.path(output_path, database, cohort_name, strata_dir)
  }


  # create directory if it does not exist
  if(!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  #create baseline covariates
  glue::glue("Generating baseline characteristics for {cyan(cohort_name)}") %>%
    cli::cat_rule(center = .)
  cli::cat_rule(crayon::yellow("Building baseline demographic covariates"))
  baseline_demographics_cat(execution_settings = execution_settings,
                            target_cohort_id = target_cohort_id,
                            output_path = output_path)
  cli::cat_rule(crayon::yellow("Building baseline continuous covariates"))
  baseline_continuous(execution_settings = execution_settings,
                      target_cohort_id = target_cohort_id,
                      output_path = output_path)

  cli::cat_rule(crayon::yellow("Building baseline cohort covariates"))
  baseline_cohorts(execution_settings = execution_settings,
                   target_cohort_id = target_cohort_id,
                   covariate_cohort_ids = df$cohort_id,
                   covariate_cohort_names = df$cohort_name,
                   output_path = output_path)

  cli::cat_rule(crayon::yellow("Building baseline condition covariates"))
  baseline_conditions(execution_settings = execution_settings,
                      target_cohort_id = target_cohort_id,
                      output_path = output_path)

  cli::cat_rule(crayon::yellow("Building baseline drug covariates"))
  baseline_drugs(execution_settings = execution_settings,
                 target_cohort_id = target_cohort_id,
                 output_path = output_path)

  invisible(df)

}


