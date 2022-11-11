#cohort_meta <- readr::read_rds(here::here("output/cohort_build/cohort_meta.rds"))
baseline_demographics_cat <- function(execution_settings,
                                      target_cohort_id,
                                      strata_id = NULL,
                                      output_path) {

  #preset feature extraction for demographic categories
  cov_settings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE,
    useDemographicsAgeGroup = TRUE,
    useDemographicsRace = TRUE,
    useDemographicsIndexYear = TRUE
  )

  #extract data
  cli::cat_bullet(crayon::yellow("Building Features using {FeatureExtraction}"),
                  bullet = "continue", bullet_col = "yellow")

  cov <- suppressMessages(
    FeatureExtraction::getDbCovariateData(
      connectionDetails = execution_settings$connectionDetails,
      cdmDatabaseSchema = execution_settings$cdm_schema,
      cohortTable = execution_settings$cohort_table,
      cohortDatabaseSchema = execution_settings$write_schema,
      cohortId = target_cohort_id,
      covariateSettings = cov_settings
    )
  )

  #get sqlite connection

  tik <- Sys.time()

  con <- DBI::dbConnect(RSQLite::SQLite(), cov@dbname)
  on.exit(DBI::dbDisconnect(con))

  cli::cat_bullet(crayon::yellow("Setting up Strata"),
                  bullet = "continue", bullet_col = "yellow")
  if(!is.null(strata_id)) {
    #get strata connection
    con_strata <- DBI::dbConnect(
      drv = get_driver(execution_settings)(),
      host = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[1]],
      dbname = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[2]],
      user = execution_settings$connectionDetails$user(),
      password = execution_settings$connectionDetails$password(),
      port = execution_settings$connectionDetails$port()
    )
    on.exit(DBI::dbDisconnect(con_strata))
    strata_table <- paste(execution_settings$cohort_table, "strata", sep = "_")
    #get strata table
    strata_tbl <- dplyr::tbl(
      con_strata,
      dbplyr::in_schema(execution_settings$write_schema, strata_table)) %>%
      dplyr::filter(strata_id == !!strata_id) %>%
      #write to sql lite db
      dplyr::copy_to(
        dest = con,
        df = .,
        name = "strata",
        overwrite = TRUE
      )
  } else{
    strata_tbl <- dplyr::tbl(con, "covariates") %>%
      distinct(rowId) %>%
      mutate(strata = 1) %>%
      rename(subject_id = rowId)
  }


  #get the denominator for the strata counts
  denom <- strata_tbl %>%
    group_by(strata) %>%
    count(name = "tot")


  #aggregate and formate data
  cli::cat_bullet(crayon::yellow("Aggregating Features"),
                  bullet = "continue", bullet_col = "yellow")
  dem_cat_tbl <- dplyr::tbl(con, "covariates") %>%
    dplyr::left_join(strata_tbl, by = c("rowId" = "subject_id")) %>%
    dplyr::mutate(
      conceptId = dbplyr::sql("CAST(SUBSTRING(covariateId, 0, LENGTH(covariateId) - 4) AS INT)"),
      analysisId = dbplyr::sql("CAST(SUBSTRING(covariateId, LENGTH(covariateId) - 4, LENGTH(covariateId)) AS INT)")
    ) %>%
    dplyr::group_by(analysisId, conceptId, strata) %>%
    dplyr::summarize(nn = sum(covariateValue, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      conceptName = dplyr::case_when(
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

  tok <- Sys.time() - tik
  cli::cat_line(crayon::yellow(glue::glue("   Process took {round(tok, digits = 2)} seconds")))
  #save output
  cli::cat_bullet(crayon::yellow("Saving Data"),
                  bullet = "continue", bullet_col = "yellow")
  save_path <- file.path(output_path, "demographics_baseline.csv")

  arrow::write_csv_arrow(dem_cat_tbl, sink = save_path)
  usethis::ui_info("Baseline demographic covariates written as a csv file to:
                   {ui_field(save_path)}")

  #exit
  invisible(dem_cat_tbl)

}



baseline_continuous <- function(execution_settings,
                                cohort_meta,
                                target_cohort_id,
                                strata_id,
                                output_path) {

  #preset feature extraction for demographic categories
  cov_settings <- FeatureExtraction::createCovariateSettings(
    useDemographicsAge = TRUE,
    useDemographicsPriorObservationTime = TRUE,
    useDemographicsPostObservationTime = TRUE,
    useDemographicsTimeInCohort = TRUE,
    useDcsi = TRUE
  )

  #extract data
  cli::cat_bullet(crayon::yellow("Building Features using {FeatureExtraction}"),
                  bullet = "continue", bullet_col = "yellow")
  cov <- suppressMessages(
    FeatureExtraction::getDbCovariateData(
      connectionDetails = execution_settings$connectionDetails,
      cdmDatabaseSchema = execution_settings$cdm_schema,
      cohortTable = execution_settings$cohort_table,
      cohortDatabaseSchema = execution_settings$write_schema,
      cohortId = target_cohort_id,
      covariateSettings = cov_settings
    )
  )

  #get sqlite connection


  tik <- Sys.time()
  con <- DBI::dbConnect(RSQLite::SQLite(), cov@dbname)
  on.exit(DBI::dbDisconnect(con))

  cli::cat_bullet(crayon::yellow("Setting up Strata"),
                  bullet = "continue", bullet_col = "yellow")
  if(!is.null(strata_id)) {
    #get strata connection
    con_strata <- DBI::dbConnect(
      drv = get_driver(execution_settings)(),
      host = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[1]],
      dbname = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[2]],
      user = execution_settings$connectionDetails$user(),
      password = execution_settings$connectionDetails$password(),
      port = execution_settings$connectionDetails$port()
    )
    on.exit(DBI::dbDisconnect(con_strata))
    strata_table <- paste(execution_settings$cohort_table, "strata", sep = "_")
    #get strata table
    strata_tbl <- dplyr::tbl(
      con_strata,
      dbplyr::in_schema(execution_settings$write_schema, strata_table)) %>%
      dplyr::filter(strata_id == !!strata_id) %>%
      #write to sql lite db
      dplyr::copy_to(
        dest = con,
        df = .,
        name = "strata",
        overwrite = TRUE
      )
  } else{
    strata_tbl <- dplyr::tbl(con, "covariates") %>%
      distinct(rowId) %>%
      mutate(strata = 1) %>%
      rename(subject_id = rowId)
  }



  cli::cat_bullet(crayon::yellow("Aggregating Features"),
                  bullet = "continue", bullet_col = "yellow")
  #aggregate and formate data
  cts_tbl <-  dplyr::tbl(con, "covariates") %>%
    dplyr::left_join(dplyr::tbl(con, "covariateRef"), by = c("covariateId")) %>%
    dplyr::left_join(dplyr::tbl(con, "analysisRef"), by = c("analysisId")) %>%
    dplyr::left_join(strata_tbl, by = c("rowId" = "subject_id")) %>%
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

  tok <- Sys.time() - tik
  cli::cat_line(crayon::yellow(glue::glue("   Process took {round(tok, digits = 2)} seconds")))
  #save output
  cli::cat_bullet(crayon::yellow("Saving Data"),
                  bullet = "continue", bullet_col = "yellow")
  save_path <- file.path(output_path, "continuous_baseline.csv")

  arrow::write_csv_arrow(cts_tbl , sink = save_path)
  usethis::ui_info("Baseline continuous covariates written as a csv file to {ui_field(save_path)}")

  #exit
  invisible(cts_tbl)
}

baseline_conditions <- function(execution_settings,
                                target_cohort_id,
                                strata_id,
                                output_path) {

  #preset feature extraction for demographic categories
  cov_settings <- FeatureExtraction::createCovariateSettings(
    useConditionGroupEraLongTerm = TRUE
  )

  #extract data
  cli::cat_bullet(crayon::yellow("Building Features using {FeatureExtraction}"),
                  bullet = "continue", bullet_col = "yellow")
  cov <- suppressMessages(
    FeatureExtraction::getDbCovariateData(
      connectionDetails = execution_settings$connectionDetails,
      cdmDatabaseSchema = execution_settings$cdm_schema,
      cohortTable = execution_settings$cohort_table,
      cohortDatabaseSchema = execution_settings$write_schema,
      cohortId = target_cohort_id,
      covariateSettings = cov_settings
    )
  )

  #get sqlite connection

  tik <- Sys.time()
  con <- DBI::dbConnect(RSQLite::SQLite(), cov@dbname)
  on.exit(DBI::dbDisconnect(con))

  cli::cat_bullet(crayon::yellow("Setting up Strata"),
                  bullet = "continue", bullet_col = "yellow")
  if(!is.null(strata_id)) {
    #get strata connection
    con_strata <- DBI::dbConnect(
      drv = get_driver(execution_settings)(),
      host = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[1]],
      dbname = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[2]],
      user = execution_settings$connectionDetails$user(),
      password = execution_settings$connectionDetails$password(),
      port = execution_settings$connectionDetails$port()
    )
    on.exit(DBI::dbDisconnect(con_strata))
    strata_table <- paste(execution_settings$cohort_table, "strata", sep = "_")
    #get strata table
    strata_tbl <- dplyr::tbl(
      con_strata,
      dbplyr::in_schema(execution_settings$write_schema, strata_table)) %>%
      dplyr::filter(strata_id == !!strata_id) %>%
      #write to sql lite db
      dplyr::copy_to(
        dest = con,
        df = .,
        name = "strata",
        overwrite = TRUE
      )
  } else{
    strata_tbl <- dplyr::tbl(con, "covariates") %>%
      distinct(rowId) %>%
      mutate(strata = 1) %>%
      rename(subject_id = rowId)
  }


  #get the denominator for the strata counts
  denom <- strata_tbl %>%
    group_by(strata) %>%
    count(name = "tot")

  cli::cat_bullet(crayon::yellow("Retrieving ICD Rollup"),
                  bullet = "continue", bullet_col = "yellow")
  #collect all concept ids used
  icd_grp <- rollupConditions(
    connectionDetails = execution_settings$connectionDetails,
    cdmDatabaseSchema = execution_settings$cdm_schema,
    conceptIds = dplyr::tbl(con, "covariateRef") %>%
      dplyr::pull(conceptId) %>%
      as.integer()) %>%
    left_join(icd_chapters(), by = c("categoryId" = "conceptId")) %>%
    select(conceptId, conceptName, categoryId, groupName, chapterName) %>%
    dplyr::copy_to(con, ., name = "icd", overwrite = TRUE)



  #aggregate and formate data for raw format
  cli::cat_bullet(crayon::yellow("Aggregating Features"),
                  bullet = "continue", bullet_col = "yellow")
  conditions_tbl <- dplyr::tbl(con, "covariates") %>%
    dplyr::left_join(strata_tbl, by = c("rowId" = "subject_id")) %>%
    dplyr::mutate(
      conceptId = dplyr::sql("CAST(SUBSTRING(covariateId, 0, LENGTH(covariateId) - 4) AS INT)")) %>%
    dplyr::group_by(conceptId, strata) %>%
    dplyr::summarize(nn = sum(covariateValue, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(icd_grp, by = c("conceptId")) %>%
    dplyr::mutate(domain = "Conditions Raw") %>%
    dplyr::select(domain, chapterName, groupName, conceptName,
           conceptId, strata, nn) %>%
    dplyr::union(
      dplyr::tbl(con, "covariates") %>%
        dplyr::left_join(strata_tbl, by = c("rowId" = "subject_id")) %>%
        dplyr::mutate(conceptId = sql("CAST(SUBSTRING(covariateId, 0, LENGTH(covariateId) - 4) AS INT)")) %>%
        dplyr::left_join(icd_grp, by = c("conceptId")) %>%
        dplyr::distinct(rowId, categoryId, groupName, chapterName, strata) %>%
        dplyr::group_by(categoryId, groupName, chapterName, strata) %>%
        dplyr::count(name = "nn") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(nn = as.double(nn),
                      domain = "Conditions Group",
                      conceptId = as.integer(categoryId),
                      conceptName = groupName) %>%
        dplyr::select(domain, chapterName, groupName, conceptName,
                      conceptId, strata, nn)
    ) %>%
    dplyr::left_join(denom, by = c("strata")) %>%
    dplyr::mutate(pct_raw = nn / tot) %>%
    dplyr::select(domain, chapterName, groupName, conceptName,
                  conceptId, strata, nn, pct_raw) %>%
    dplyr::collect()

  tok <- Sys.time() - tik
  cli::cat_line(crayon::yellow(glue::glue("   Process took {round(tok, digits = 2)} seconds")))
  #save output
  cli::cat_bullet(crayon::yellow("Saving Data"),
                  bullet = "continue", bullet_col = "yellow")
  save_path <- file.path(output_path, "conditions_baseline.csv")

  arrow::write_csv_arrow(conditions_tbl, sink = save_path)
  usethis::ui_info("Baseline condition covariates written as a csv file to {ui_field(save_path)}")

  invisible(conditions_tbl)
}


baseline_drugs <- function(execution_settings,
                           target_cohort_id,
                           strata_id,
                           output_path) {

  #preset feature extraction for demographic categories
  cov_settings <- FeatureExtraction::createCovariateSettings(
    useDrugGroupEraLongTerm = TRUE,
    excludedCovariateConceptIds = c(21600001, 21600959, 21601237, #remove ATC 1st class
                                    21601907, 21602359, 21602681,
                                    21602795, 21601386, 21603931,
                                    21604180, 21604847, 21605007,
                                    21603550, 21605212)
  )

  #extract data
  cli::cat_bullet(crayon::yellow("Building Features using {FeatureExtraction}"),
                  bullet = "continue", bullet_col = "yellow")
  cov <- suppressMessages(
    FeatureExtraction::getDbCovariateData(
      connectionDetails = execution_settings$connectionDetails,
      cdmDatabaseSchema = execution_settings$cdm_schema,
      cohortTable = execution_settings$cohort_table,
      cohortDatabaseSchema = execution_settings$write_schema,
      cohortId = target_cohort_id,
      covariateSettings = cov_settings
    )
  )

  #get sqlite connection
  tik <- Sys.time()
  con <- DBI::dbConnect(RSQLite::SQLite(), cov@dbname)
  on.exit(DBI::dbDisconnect(con))

  cli::cat_bullet(crayon::yellow("Setting up Strata"),
                  bullet = "continue", bullet_col = "yellow")
  if(!is.null(strata_id)) {
    #get strata connection
    con_strata <- DBI::dbConnect(
      drv = get_driver(execution_settings)(),
      host = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[1]],
      dbname = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[2]],
      user = execution_settings$connectionDetails$user(),
      password = execution_settings$connectionDetails$password(),
      port = execution_settings$connectionDetails$port()
    )
    on.exit(DBI::dbDisconnect(con_strata))
    strata_table <- paste(execution_settings$cohort_table, "strata", sep = "_")
    #get strata table
    strata_tbl <- dplyr::tbl(
      con_strata,
      dbplyr::in_schema(execution_settings$write_schema, strata_table)) %>%
      dplyr::filter(strata_id == !!strata_id) %>%
      #write to sql lite db
      dplyr::copy_to(
        dest = con,
        df = .,
        name = "strata",
        overwrite = TRUE
      )
  } else{
    strata_tbl <- dplyr::tbl(con, "covariates") %>%
      distinct(rowId) %>%
      mutate(strata = 1) %>%
      rename(subject_id = rowId)
  }


  #get the denominator for the strata counts
  denom <- strata_tbl %>%
    group_by(strata) %>%
    count(name = "tot")

  cli::cat_bullet(crayon::yellow("Retrieving ATC Rollup"),
                  bullet = "continue", bullet_col = "yellow")
  #collect all concept ids used
  atc_grp <- rollupDrugs(
    connectionDetails = execution_settings$connectionDetails,
    cdmDatabaseSchema = execution_settings$cdm_schema,
    conceptIds = dplyr::tbl(con, "covariateRef") %>%
      dplyr::pull(conceptId) %>%
      as.integer()) %>%
    dplyr::rename(groupName = categoryName) %>%
    dplyr::mutate(chapterName = groupName) %>%
    dplyr::copy_to(con, ., name = "atc", overwrite = TRUE)


  cli::cat_bullet(crayon::yellow("Aggregating Features"),
                  bullet = "continue", bullet_col = "yellow")
  #aggregate and formate data
  drugs_tbl <- dplyr::tbl(con, "covariates") %>%
    dplyr::left_join(strata_tbl, by = c("rowId" = "subject_id")) %>%
    dplyr::mutate(
      conceptId = dplyr::sql("CAST(SUBSTRING(covariateId, 0, LENGTH(covariateId) - 4) AS INT)")
      ) %>%
    dplyr::group_by(conceptId, strata) %>%
    dplyr::summarize(nn = sum(covariateValue, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(atc_grp, by = c("conceptId")) %>%
    dplyr::mutate(domain = "Drugs Raw") %>%
    dplyr::left_join(denom, by = c("strata")) %>%
    dplyr::mutate(pct_raw = nn / tot) %>%
    dplyr::select(domain, chapterName, groupName,
           conceptName, conceptId,
           strata, nn, pct_raw) %>%
    dplyr::collect()

  tok <- Sys.time() - tik
  cli::cat_line(crayon::yellow(glue::glue("   Process took {round(tok, digits = 2)} seconds")))
  #save output
  cli::cat_bullet(crayon::yellow("Saving Data"),
                  bullet = "continue", bullet_col = "yellow")
  save_path <- file.path(output_path, "drugs_baseline.csv")

  arrow::write_csv_arrow(drugs_tbl, sink = save_path)
  usethis::ui_info("Baseline drug covariates written as a csv file to {ui_field(save_path)}")

  invisible(drugs_tbl)
}


baseline_cohorts <- function(execution_settings,
                             target_cohort_id,
                             covariate_cohort_ids,
                             covariate_cohort_names,
                             strata_id,
                             output_path) {


  #determine the correct driver either postgres or redshift
  driver <- execution_settings$connectionDetails$dbms
  drv <- switch(driver,
                postgresql = RPostgres::Postgres,
                redshift = RPostgres::Redshift)

  tik <- Sys.time()

  #connect to dbms
  con <- DBI::dbConnect(
    drv = drv(),
    host = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[1]],
    dbname = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[2]],
    user = execution_settings$connectionDetails$user(),
    password = execution_settings$connectionDetails$password(),
    port = execution_settings$connectionDetails$port()
  )
  on.exit(DBI::dbDisconnect(con))


  cohort_schema <- execution_settings$write_schema
  cohort_table <- execution_settings$cohort_table

  cohort_key <- tibble::tibble(
    cohort_id = as.integer(covariate_cohort_ids),
    cohort_name = covariate_cohort_names
  )
  cli::cat_bullet(crayon::yellow("Retrieving target cohort"),
                  bullet = "continue", bullet_col = "yellow")
  target_cohort_table <- dplyr::tbl(con, dbplyr::in_schema(cohort_schema, cohort_table)) %>%
    filter(cohort_definition_id == target_cohort_id)


  cli::cat_bullet(crayon::yellow("Setting up Strata"),
                  bullet = "continue", bullet_col = "yellow")
  if(!is.null(strata_id)) {
    strata_table <- paste(execution_settings$cohort_table, "strata", sep = "_")
    #get strata table
    strata_tbl <- dplyr::tbl(
      con,
      dbplyr::in_schema(execution_settings$write_schema, strata_table)) %>%
      dplyr::filter(strata_id == !!strata_id) %>%
      #write to sql lite db
      dplyr::copy_to(
        dest = con,
        df = .,
        name = "strata",
        overwrite = TRUE
      )
  } else{
    strata_tbl <- target_cohort_table %>%
      distinct(subject_id) %>%
      mutate(strata = 1)
  }


  #get the denominator for the strata counts
  denom <- strata_tbl %>%
    group_by(strata) %>%
    count(name = "tot")


  cli::cat_bullet(crayon::yellow("Retrieving covariate cohorts"),
                  bullet = "continue", bullet_col = "yellow")
  covariate_cohort_table <- dplyr::tbl(con, dbplyr::in_schema(cohort_schema, cohort_table)) %>%
    filter(cohort_definition_id %in% covariate_cohort_ids)



  cli::cat_bullet(crayon::yellow("Aggregating Features from cohort table"),
                  bullet = "continue", bullet_col = "yellow")
  cohort_tbl <- target_cohort_table %>%
    dplyr::left_join(strata_tbl, by = c("subject_id", "cohort_definition_id")) %>%
    dplyr::inner_join(covariate_cohort_table, by = c("subject_id"),
                      suffix = c("_target", "_covariate")) %>%
    dplyr::mutate(shift = cohort_start_date_target - lubridate::days(365),
                  hit = dplyr::if_else(shift <= cohort_start_date_covariate, 1L, 0L, 0L)) %>%
    dplyr::group_by(cohort_definition_id_covariate, strata) %>%
    dplyr::summarize(nn = sum(hit, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(nn = as.double(nn),
                  cohort_id = as.integer(cohort_definition_id_covariate)) %>%
    dplyr::left_join(denom, by = c("strata")) %>%
    dplyr::select(-cohort_definition_id_covariate) %>%
    dplyr::mutate(pct_raw = nn / tot) %>%
    dplyr::arrange(cohort_id, strata) %>%
    dplyr::collect() %>%
    dplyr::right_join(cohort_key, by = c("cohort_id")) %>%
    dplyr::mutate(
      domain = "Cohorts",
      chapter_name = "Conditions",
      group_name = "Conditions"
    ) %>%
    dplyr::select(domain, chapter_name, group_name,
                  cohort_name, cohort_id,
                  strata, nn, pct_raw)

  tok <- Sys.time() - tik
  cli::cat_line(crayon::yellow(glue::glue("   Process took {round(tok, digits = 2)} seconds")))
  #save output
  cli::cat_bullet(crayon::yellow("Saving Data"),
                  bullet = "continue", bullet_col = "yellow")
  save_path <- file.path(output_path, "cohorts_baseline.csv")

  arrow::write_csv_arrow(cohort_tbl, sink = save_path)
  usethis::ui_info("Baseline cohort covariates written as a csv file to {ui_field(save_path)}")

  invisible(cohort_tbl)
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
#' @param target_cohort_name the name of the target cohort
#' @param cohort_meta file that specifies all cohorts created for the study.
#' @param strata_id an integer specifying the strata id used
#' @param strata_name the name of the strata
#' @param output_path the output path to save the file. Default output/baseline
#' @import magrittr
#' @export
generate_baseline_covariates <- function(execution_settings,
                                         target_cohort_id,
                                         target_cohort_name,
                                         strata_id = NULL,
                                         strata_name = NULL,
                                         output_path) {



  ## Set up save path -------------------------------
  if(is.null(strata_name)) {
    strata_name <- "totals"
  }

  #create the file path string
  output_path <- file.path(output_path,
                           execution_settings$databaseId,
                           target_cohort_name,
                           strata_name)

  # create directory if it does not exist
  if(!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }


  #create baseline covariates ---------------------------------
  ParallelLogger::logInfo(
    "- Generating baseline characteristics\n\t",
    "> Cohort ",target_cohort_id, ": ", crayon::yellow(target_cohort_name),
    "\n\t",
    "> Strata ", strata_id, ": ", crayon::yellow(strata_name)
  )

  cli::cat_bullet(crayon::red("Building demographic covariates"))
  baseline_demographics_cat(execution_settings = execution_settings,
                            target_cohort_id = target_cohort_id,
                            strata_id = strata_id,
                            output_path = output_path)
  cli::cat_bullet(crayon::red("Building continuous covariates"))
  baseline_continuous(execution_settings = execution_settings,
                      target_cohort_id = target_cohort_id,
                      strata_id = strata_id,
                      output_path = output_path)


  #cli::cat_bullet(crayon::red("Building cohort covariates"))

  #get the ids of the cohort definitions used as covariates
  # cohort_covariates <- cohort_meta %>%
  #   filter(grepl("covariates", cohort_file) | grepl("strata", cohort_file)) %>%
  #   select(cohort_id, cohort_name) %>%
  #   mutate(cohort_name = gsub("covariates_", "", cohort_name),
  #          cohort_name = gsub("strata_", "", cohort_name))

  # if
  #
  # baseline_cohorts(execution_settings = execution_settings,
  #                  target_cohort_id = target_cohort_id,
  #                  strata_id = strata_id,
  #                  covariate_cohort_ids = cohort_covariates$cohort_id,
  #                  covariate_cohort_names = cohort_covariates$cohort_name,
  #                  output_path = output_path)

  cli::cat_bullet(crayon::red("Building condition covariates"))
  baseline_conditions(execution_settings = execution_settings,
                      target_cohort_id = target_cohort_id,
                      strata_id = strata_id,
                      output_path = output_path)

  cli::cat_bullet(crayon::red("Building drug covariates"))
  baseline_drugs(execution_settings = execution_settings,
                 target_cohort_id = target_cohort_id,
                 strata_id = strata_id,
                 output_path = output_path)

  cli::cat_rule()
  invisible(df)

}



#' Function to run baseline characteristics
#'
#' @param execution_settings executionSettings object with connection credentials
#' @param cohort_meta file that specifies all cohorts created for the study.
#' @param strata_meta a tibble generated from build_boris_strata
#' @include strata.R
#' @export
run_baseline_analysis <- function(execution_settings,
                                  cohort_meta,
                                  strata_meta) {

  exposure_cohorts <- cohort_meta %>%
    dplyr::filter(grepl("exposure",cohort_name)) %>%
    dplyr::select(cohort_id, cohort_name) %>%
    dplyr::mutate(
      cohort_name = gsub("exposure_", "", cohort_name),
      cohort_id = as.integer(cohort_id))

  analysis_manifest <- strata_meta %>%
    dplyr::left_join(exposure_cohorts, by = c("cohort_definition_id" = "cohort_id")) %>%
    dplyr::select(cohort_definition_id,
                  cohort_name,
                  strata_id,
                  strata_name)


  #baseline covariates
  purrr::pwalk(analysis_manifest,
              ~generate_baseline_covariates(
                execution_settings = execution_settings,
                target_cohort_id = ..1,
                target_cohort_name = ..2,
                strata_id = ..3,
                strata_name = ..4,
                cohort_meta = cohort_meta,
                output_path = "output/baseline"
              ))

  invisible(exposure_cohorts)

}

