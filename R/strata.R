
#' Function to create a gender strata
#'
#' @param execution_settings executionSettings object with connection credentials
#' @param target_cohort_id an integer specifying the cohort id to use. Please review
#' the cohort_meta file in output/cohort_build to determine the cohort definition ids
#' @param value the concept id for the gender strata. default is 8532L
#' @import magrittr
#' @export
gender_strata <- function(execution_settings,
                          target_cohort_id,
                          value = 8532L) {

  strata_settings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE
  )


  cov <- FeatureExtraction::getDbCovariateData(connectionDetails = execution_settings$connectionDetails,
                                               cdmDatabaseSchema = execution_settings$cdm_schema,
                                               cohortTable = execution_settings$cohort_table,
                                               cohortDatabaseSchema = execution_settings$write_schema,
                                               cohortId = target_cohort_id,
                                               covariateSettings = strata_settings)

  #get sqlite connection
  sqlite <- RSQLite::SQLite()
  con <- DBI::dbConnect(sqlite, cov@dbname)


  df <- dplyr::tbl(con, "covariates") %>%
    dplyr::mutate(conceptId = dbplyr::sql("CAST(SUBSTRING(covariateId, 0, LENGTH(covariateId) - 4) AS INT)"),
                  analysisId = dbplyr::sql("CAST(SUBSTRING(covariateId, LENGTH(covariateId) - 4, LENGTH(covariateId)) AS INT)")) %>%
    dplyr::filter(analysisId == 1L) %>%
    dplyr::mutate(strata = dplyr::if_else(conceptId == value, 1L, 0L, 0L)) %>%
    dplyr::select(.data$rowId, .data$strata) %>%
    dplyr::collect()

  strata <- structure(
    list(
      data = df,
      strata_name = paste("gender", value, sep = "_")
    ),
    class = "strata"
  )

  return(strata)
  DBI::dbDisconnect(con)
}

#' Function to create an age strata
#'
#' @param execution_settings executionSettings object with connection credentials
#' @param target_cohort_id an integer specifying the cohort id to use. Please review
#' the cohort_meta file in output/cohort_build to determine the cohort definition ids
#' @param value the age value used as a cutoff, defaults to 65 years old
#' @import magrittr
#' @export
age_strata <- function(execution_settings,
                       target_cohort_id,
                       value = 65L) {

  strata_settings <- FeatureExtraction::createCovariateSettings(
    useDemographicsAge = TRUE,
  )


  cov <- FeatureExtraction::getDbCovariateData(connectionDetails = execution_settings$connectionDetails,
                                               cdmDatabaseSchema = execution_settings$cdm_schema,
                                               cohortTable = execution_settings$cohort_table,
                                               cohortDatabaseSchema = execution_settings$write_schema,
                                               cohortId = target_cohort_id,
                                               covariateSettings = strata_settings)

  #get sqlite connection
  sqlite <- RSQLite::SQLite()
  con <- DBI::dbConnect(sqlite, cov@dbname)

  df <- dplyr::tbl(con, "covariates") %>%
    dplyr::mutate(conceptId = dbplyr::sql("CAST(SUBSTRING(covariateId, 0, LENGTH(covariateId) - 4) AS INT)"),
                  analysisId = dbplyr::sql("CAST(SUBSTRING(covariateId, LENGTH(covariateId) - 4, LENGTH(covariateId)) AS INT)")) %>%
    dplyr::filter(analysisId == 2L) %>%
    dplyr::mutate(strata = dplyr::if_else(covariateValue >= value, 1L, 0L, 0L)) %>%
    dplyr::select(.data$rowId, .data$strata) %>%
    dplyr::collect()


  strata <- structure(
    list(
      data = df,
      strata_name = paste("age", value, sep = "_")
    ),
    class = "strata"
  )

  return(strata)
  DBI::dbDisconnect(con)
}


#' Function to create a date strata
#'
#' @param execution_settings executionSettings object with connection credentials
#' @param target_cohort_id an integer specifying the cohort id to use. Please review
#' the cohort_meta file in output/cohort_build to determine the cohort definition ids
#' @param value the date value used as a cutoff, this must be an integer in format yyyymm
#' The default value is 202003L (March 2020 the threshold for covid)
#' @import magrittr
#' @export
date_strata <- function(execution_settings,
                        target_cohort_id,
                        value = 202003L) {

  strata_settings <- FeatureExtraction::createCovariateSettings(
    useDemographicsIndexYearMonth = TRUE
  )


  cov <- FeatureExtraction::getDbCovariateData(connectionDetails = execution_settings$connectionDetails,
                                               cdmDatabaseSchema = execution_settings$cdm_schema,
                                               cohortTable = execution_settings$cohort_table,
                                               cohortDatabaseSchema = execution_settings$write_schema,
                                               cohortId = target_cohort_id,
                                               covariateSettings = strata_settings)

  #get sqlite connection
  sqlite <- RSQLite::SQLite()
  con <- DBI::dbConnect(sqlite, cov@dbname)

  df <- dplyr::tbl(con, "covariates") %>%
    dplyr::mutate(conceptId = dbplyr::sql("CAST(SUBSTRING(covariateId, 0, LENGTH(covariateId) - 4) AS INT)"),
                  analysisId = dbplyr::sql("CAST(SUBSTRING(covariateId, LENGTH(covariateId) - 4, LENGTH(covariateId)) AS INT)")) %>%
    filter(analysisId == 11L) %>%
    dplyr::mutate(strata = dplyr::if_else(conceptId >= value, 1L, 0L, 0L)) %>%
    dplyr::select(.data$rowId, .data$strata) %>%
    dplyr::collect()

  strata <- structure(
    list(
      data = df,
      strata_name = paste("date", value, sep = "_")
    ),
    class = "strata"
  )

  return(strata)
  DBI::dbDisconnect(con)
}


#' Function to create a cohort strata
#'
#' Note that the cohort strata is based on a 365 lookback period. The stratifying
#' condition must have occurred within 365 days prior to the start of the target cohort.
#'
#' @param execution_settings executionSettings object with connection credentials
#' @param target_cohort_id an integer specifying the cohort id to use. Please review
#' the cohort_meta file in output/cohort_build to determine the cohort definition ids
#' @param value the cohort definition id to stratify. Please review
#' the cohort_meta file in output/cohort_build to determine the cohort definition ids.
#' The default value is 17L (strata/t2d)
#' @import magrittr
#' @export
cohort_strata <- function(execution_settings,
                          target_cohort_id,
                          value = 17L) {

  #determine the correct driver either postgres or redshift
  driver <- execution_settings$connectionDetails$dbms
  drv <- switch(driver,
                postgresql = RPostgres::Postgres,
                redshift = RPostgres::Redshift)

  #connect to dbms
  con <- DBI::dbConnect(
    drv = drv,
    host = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[1]],
    dbname = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[2]],
    user = execution_settings$connectionDetails$user(),
    password = execution_settings$connectionDetails$password(),
    port = execution_settings$connectionDetails$port()
  )
  #set schema and table for cohorts
  cohort_schema <- execution_settings$write_schema
  cohort_table <- execution_settings$cohort_table

  # get target cohort
  target_cohort_table <- dplyr::tbl(con, dbplyr::in_schema(cohort_schema, cohort_table)) %>%
    filter(cohort_definition_id == target_cohort_id)

  #get stratification cohort
  strata_cohort_table <- dplyr::tbl(con, dbplyr::in_schema(cohort_schema, cohort_table)) %>%
    filter(cohort_definition_id == value)

  #create the strata
  df <- target_cohort_table %>%
    dplyr::left_join(strata_cohort_table, by = c("subject_id"),
                      suffix = c("_target", "_strata")) %>%
    dplyr::mutate(shift = cohort_start_date_target - lubridate::days(365),
                  strata = dplyr::if_else(shift <= cohort_start_date_strata, 1L, 0L, 0L),
                  rowId = subject_id) %>%
    dplyr::select(.data$rowId, .data$strata) %>%
    collect()

  strata <- structure(
    list(
      data = df,
      strata_name = paste("cohort", value, sep = "_")
    ),
    class = "strata"
  )

  return(strata)
  DBI::dbDisconnect(con)


}
