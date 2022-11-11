create_strata_schema <- function(execution_settings) {

  strata_table <- paste(execution_settings$cohort_table, "strata", sep = "_")
  schema_table_name <- paste(execution_settings$write_schema, strata_table, sep = ".")


  connection <- DatabaseConnector::connect(execution_settings$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))


  ParallelLogger::logInfo("Drop strata table if exists")

  "IF OBJECT_ID('@write_schema.@strata_table', 'U') IS NOT NULL
  DROP TABLE @write_schema.@strata_table;" %>%
    SqlRender::render(
      write_schema = execution_settings$write_schema,
      strata_table = strata_table
    ) %>%
    SqlRender::translate(execution_settings$connectionDetails$dbms) %>%
    DatabaseConnector::executeSql(connection = connection,
                                  sql = .)
  ParallelLogger::logInfo("Create strata table: ", crayon::green(schema_table_name))

  "CREATE TABLE @write_schema.@strata_table (
    strata_id INT,
    cohort_definition_id BIGINT,
    subject_id BIGINT,
    strata INT
  );"  %>%
    SqlRender::render(
      write_schema = execution_settings$write_schema,
      strata_table = strata_table
    ) %>%
    SqlRender::translate(execution_settings$connectionDetails$dbms) %>%
    DatabaseConnector::executeSql(connection = connection,
                                  sql = .)

  invisible(strata_table)

}


#' Function to create a gender strata
#'
#' @param execution_settings executionSettings object with connection credentials
#' @param target_cohort_id an integer specifying the cohort id to use. Please review
#' the cohort_meta file in output/cohort_build to determine the cohort definition ids
#' @param strata_id an integer identifying the strata
#' @param strata_name the name of the strata, default female
#' @param strata_table the table where stratas are stored in the write_schema of database
#' @import magrittr
#' @include helpers.R
#' @export
female_strata <- function(execution_settings,
                          target_cohort_id,
                          strata_id,
                          strata_name = "female",
                          strata_table) {


  #connect to dbms
  con <- DBI::dbConnect(
    drv = get_driver(execution_settings)(),
    host = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[1]],
    dbname = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[2]],
    user = execution_settings$connectionDetails$user(),
    password = execution_settings$connectionDetails$password(),
    port = execution_settings$connectionDetails$port()
  )

  on.exit(DBI::dbDisconnect(con))

  #set schema and table for cohorts
  cdm_schema <- execution_settings$cdm_schema


  ParallelLogger::logInfo("Building Female Strata for cohort ", crayon::blue(target_cohort_id))
  tik <- Sys.time()
  df <- get_cohort(con = con,
                   execution_settings = execution_settings,
                   cohort_id = target_cohort_id) %>%
    dplyr::left_join(dplyr::tbl(con, dbplyr::in_schema(cdm_schema, "person")),
                     by = c("subject_id" = "person_id")) %>%
    mutate(
      strata = dplyr::if_else(gender_concept_id == 8532L, 1L, 0L, 0L), # Get femal strata
      strata_id = as.integer(strata_id)
    ) %>%
    dplyr::select(strata_id, cohort_definition_id, subject_id, strata)


  #insert into strata table
  append_to_strata_table(con = con,
                         execution_settings = execution_settings,
                         strata_table = strata_table,
                         src_df = df)

  #summarize the strata for a meta file
  res <- summarize_strata(src_df = df,
                          execution_settings = execution_settings,
                          strata_table = strata_table,
                          strata_name = strata_name)
  tok <- Sys.time() - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tok)), attr(tok, "units"))
  ParallelLogger::logInfo("Build took ", crayon::blue(tok_format))
  ParallelLogger::logInfo("\n-----------------------------------------\n")
  return(res)

}

#' Function to create a covid strata
#'
#' @param execution_settings executionSettings object with connection credentials
#' @param target_cohort_id an integer specifying the cohort id to use. Please review
#' the cohort_meta file in output/cohort_build to determine the cohort definition ids
#' @param strata_id an integer identifying the strata
#' @param strata_name the name of the strata, default covid
#' @param strata_table the table where stratas are stored in the write_schema of database
#' @import magrittr
#' @include helpers.R
#' @export
covid_strata <- function(execution_settings,
                         target_cohort_id,
                         strata_id,
                         strata_name = "covid",
                         strata_table) {

  #connect to dbms
  con <- DBI::dbConnect(
    drv = get_driver(execution_settings)(),
    host = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[1]],
    dbname = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[2]],
    user = execution_settings$connectionDetails$user(),
    password = execution_settings$connectionDetails$password(),
    port = execution_settings$connectionDetails$port()
  )

  on.exit(DBI::dbDisconnect(con))

  # get strata for covid
  ParallelLogger::logInfo("Building Covid Strata for cohort ", crayon::blue(target_cohort_id))
  tik <- Sys.time()
  df <- get_cohort(con = con,
                    execution_settings = execution_settings,
                    cohort_id = target_cohort_id) %>%
    mutate(
      #date of covid time
      strata = dplyr::if_else(cohort_start_date >= as.Date("2020-03-01"), 1L, 0L, 0L),
      strata_id = as.integer(strata_id)
    ) %>%
    dplyr::select(strata_id, cohort_definition_id, subject_id, strata)

  #insert into strata table
  append_to_strata_table(con = con,
                         execution_settings = execution_settings,
                         strata_table = strata_table,
                         src_df = df)

  #summarize the strata for a meta file
  res <- summarize_strata(src_df = df,
                          execution_settings = execution_settings,
                          strata_table = strata_table,
                          strata_name = strata_name)
  tok <- Sys.time() - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tok)), attr(tok, "units"))
  ParallelLogger::logInfo("Build took ", crayon::blue(tok_format))
  ParallelLogger::logInfo("\n-----------------------------------------\n")
  return(res)

}

#' Function to create a age strata
#'
#' @param execution_settings executionSettings object with connection credentials
#' @param target_cohort_id an integer specifying the cohort id to use. Please review
#' the cohort_meta file in output/cohort_build to determine the cohort definition ids
#' @param strata_id an integer identifying the strata
#' @param strata_name the name of the strata, default age65
#' @param strata_table the table where stratas are stored in the write_schema of database
#' @import magrittr
#' @include helpers.R
#' @export
age_strata <- function(execution_settings,
                       target_cohort_id,
                       strata_id,
                       strata_name = "age65",
                       strata_table) {


  #connect to dbms
  con <- DBI::dbConnect(
    drv = get_driver(execution_settings)(),
    host = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[1]],
    dbname = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[2]],
    user = execution_settings$connectionDetails$user(),
    password = execution_settings$connectionDetails$password(),
    port = execution_settings$connectionDetails$port()
  )

  on.exit(DBI::dbDisconnect(con))

  #set schema and table for cohorts
  cdm_schema <- execution_settings$cdm_schema

  ParallelLogger::logInfo("Building Age 65 Strata for cohort ", crayon::blue(target_cohort_id))
  tik <- Sys.time()
  df <- get_cohort(con = con,
                   execution_settings = execution_settings,
                   cohort_id = target_cohort_id) %>%
    dplyr::left_join(dplyr::tbl(con, dbplyr::in_schema(cdm_schema, "person")),
                     by = c("subject_id" = "person_id")) %>%
    dplyr::mutate(
      dob = as.Date(paste(year_of_birth, "01", "01", sep = "-")),
      age = as.integer(floor((cohort_start_date - dob) / 365.25)),
      strata = dplyr::if_else(age >= 65L, 1L, 0L, 0L),
      strata_id = as.integer(strata_id)
    ) %>%
    dplyr::select(strata_id, cohort_definition_id, subject_id, strata)

  #insert into strata table
  append_to_strata_table(con = con,
                         execution_settings = execution_settings,
                         strata_table = strata_table,
                         src_df = df)

  #summarize the strata for a meta file
  res <- summarize_strata(src_df = df,
                          execution_settings = execution_settings,
                          strata_table = strata_table,
                          strata_name = strata_name)
  tok <- Sys.time() - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tok)), attr(tok, "units"))
  ParallelLogger::logInfo("Build took ", crayon::blue(tok_format))
  ParallelLogger::logInfo("\n-----------------------------------------\n")
  return(res)

}

#' Function to create a total strata
#'
#' @param execution_settings executionSettings object with connection credentials
#' @param target_cohort_id an integer specifying the cohort id to use. Please review
#' the cohort_meta file in output/cohort_build to determine the cohort definition ids
#' @param strata_id an integer identifying the strata
#' @param strata_name the name of the strata, default total
#' @param strata_table the table where stratas are stored in the write_schema of database
#' @import magrittr
#' @include helpers.R
#' @export
total_strata <- function(execution_settings,
                         target_cohort_id,
                         strata_id,
                         strata_name = "total",
                         strata_table) {

  #connect to dbms
  con <- DBI::dbConnect(
    drv = get_driver(execution_settings)(),
    host = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[1]],
    dbname = strsplit(execution_settings$connectionDetails$server(), "/")[[1]][[2]],
    user = execution_settings$connectionDetails$user(),
    password = execution_settings$connectionDetails$password(),
    port = execution_settings$connectionDetails$port()
  )

  on.exit(DBI::dbDisconnect(con))

  #set schema and table for cohorts
  cohort_schema <- execution_settings$write_schema
  cohort_table <- execution_settings$cohort_table

  # get target cohort
  ParallelLogger::logInfo("Building Total Strata for cohort ", crayon::blue(target_cohort_id))
  tik <- Sys.time()
  df <- get_cohort(con = con,
                   execution_settings = execution_settings,
                   cohort_id = target_cohort_id) %>%
    mutate(strata = 1,
           strata_id = strata_id) %>%
    select(strata_id, cohort_definition_id, subject_id, strata)

  #insert into strata table
  append_to_strata_table(con = con,
                         execution_settings = execution_settings,
                         strata_table = strata_table,
                         src_df = df)

  #summarize the strata for a meta file
  res <- summarize_strata(src_df = df,
                          execution_settings = execution_settings,
                          strata_table = strata_table,
                          strata_name = strata_name)

  tok <- Sys.time() - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tok)), attr(tok, "units"))
  ParallelLogger::logInfo("Build took ", crayon::blue(tok_format))
  ParallelLogger::logInfo("\n-----------------------------------------\n")

  return(res)
}


create_strata <- function(execution_settings,
                          strata_manifest,
                          strata_table,
                          strata_type = c("total", "female", "covid", "age")) {


  strataFn <- switch(strata_type,
                     total = "total_strata",
                     female = "female_strata",
                     covid = "covid_strata",
                     age = "age_strata") %>%
    rlang::call2()


  tmp <- strata_manifest %>%
    filter(strata_name == !!strata_name)

}

#' Function to build strata table in write schema
#'
#' @description This function builds all strata used in the analysis
#' and places them in a strata table in the write_schema. Returns a strata_meta
#' table with information about the strata
#' @param execution_settings executionSettings object with connection credentials
#' @param target_cohort_ids the target cohorts used against strata
#' @param strata_name the names of the strata
#' @export
build_strata <- function(execution_settings,
                         target_cohort_ids,
                         strata_name) {

  strata_table <- paste(execution_settings$cohort_table, "strata", sep = "_")

  n_strata <- length(strata_name)
  n_cohorts <- length(target_cohort_ids)

  strata_manifest <- tibble::tibble(
    cohort_definition_id = rep(as.integer(target_cohort_ids), n_strata),
    strata_name = rep(strata_name, each = n_cohorts)
  ) %>%
    dplyr::mutate(strata_id = row_number(), .before = 1)


  ParallelLogger::logInfo(
  "\n--------------------------------- ",
  crayon::bgMagenta("Creating Strata for study"),
  " ---------------------------------\n")
  #create strata table
  create_strata_schema(execution_settings)


  tot <- purrr::pmap_dfr(strata_manifest %>% filter(strata_name == "total"),
                           ~total_strata(execution_settings = execution_settings,
                                         target_cohort_id = ..2,
                                         strata_id = ..1,
                                         strata_name = ..3,
                                         strata_table = strata_table))

  age65 <- purrr::pmap_dfr(strata_manifest %>% filter(strata_name == "age65"),
                           ~age_strata(execution_settings = execution_settings,
                                         target_cohort_id = ..2,
                                         strata_id = ..1,
                                         strata_name = ..3,
                                         strata_table = strata_table))

  female <- purrr::pmap_dfr(strata_manifest %>% filter(strata_name == "female"),
                            ~female_strata(execution_settings = execution_settings,
                                           target_cohort_id = ..2,
                                           strata_id = ..1,
                                           strata_name = ..3,
                                           strata_table = strata_table))

  covid <- purrr::pmap_dfr(strata_manifest %>% filter(strata_name == "covid"),
                           ~covid_strata(execution_settings = execution_settings,
                                         target_cohort_id = ..2,
                                         strata_id = ..1,
                                         strata_name = ..3,
                                         strata_table = strata_table))

  strata_meta <- bind_rows(tot, age65, female, covid)

  return(strata_meta)

}
