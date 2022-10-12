post_index_drug_cohorts <- function(execution_settings,
                                    target_cohort_id,
                                    drug_cohort_ids,
                                    startDays = c(0, 91),
                                    endDays = c(90, 365),
                                    strata,
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


#' Function that gets the post_index drugs for the analysis
#'
#'
#' @param execution_settings executionSettings object with connection credentials
#' @param target_cohort_id an integer specifying the cohort id to use. Please review
#' the cohort_meta file in output/cohort_build to determine the cohort definition ids
#' @param cohort_meta file that specifies all cohorts created for the study.
#' @param strata a dateframe from a strata function
#' @param output_path the output path to save the file. Default output/postindex
#' @import magrittr
#' @include helpers.R
#' @export
generate_post_index_drugs <- function(execution_settings,
                                      target_cohort_id,
                                      cohort_meta,
                                      strata = NULL,
                                      output_path = "output/post_index") {


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

  cli::cat_rule(crayon::yellow("Building post index drugs"))
  post_index_drug_concepts(execution_settings = execution_settings,
                           target_cohort_id = target_cohort_id,
                           strata = strata,
                           output_path = output_path)


  invisible(df)

}
