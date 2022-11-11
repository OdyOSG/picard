post_index_drug_cohorts <- function(execution_settings,
                                    target_cohort_id,
                                    strata_id,
                                    drug_cohort_ids,
                                    drug_cohort_names,
                                    startDays,
                                    endDays,
                                    output_path) {


  tik <- Sys.time()
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

  cohort_schema <- execution_settings$write_schema
  cohort_table <- execution_settings$cohort_table

  cohort_key <- tibble::tibble(
    cohort_id = as.integer(drug_cohort_ids),
    cohort_name = drug_cohort_names
  )

  #get strata table
  cli::cat_bullet(crayon::yellow("Retrieving strata table"),
                  bullet = "continue", bullet_col = "yellow")
  strata_tbl <- dplyr::tbl(
    con,
    dbplyr::in_schema(execution_settings$write_schema,
                      "boris_strata")
  ) %>%
    dplyr::filter(strata_id == !!strata_id)

  #get the denominator for the strata counts
  denom <- strata_tbl %>%
    group_by(strata) %>%
    count(name = "tot")

  #get drug tables
  cli::cat_bullet(crayon::yellow("Retrieving Drug Cohorts"),
                  bullet = "continue", bullet_col = "yellow")
  drugs_cohort_table <- dplyr::tbl(con,
                                   dbplyr::in_schema(cohort_schema,
                                                     cohort_table)) %>%
    filter(cohort_definition_id %in% drug_cohort_ids)



  #create the post index drug table
  cli::cat_bullet(crayon::yellow("Aggregating post index analysis"),
                  bullet = "continue", bullet_col = "yellow")
  post_index_tbl <- get_cohort(con = con,
                               execution_settings = execution_settings,
                               cohort_id = target_cohort_id) %>%
    dplyr::left_join(strata_tbl, by = c("subject_id", "cohort_definition_id")) %>%
    dplyr::inner_join(drugs_cohort_table, by = c("subject_id"),
                      suffix = c("_target", "_drugs")) %>%
    dplyr::mutate(winA = cohort_start_date_target + lubridate::days(startDays),
                  winB = cohort_start_date_target + lubridate::days(endDays),
                  hit = dplyr::if_else(
                    dplyr::between(cohort_start_date_drugs, winA, winB),
                    1L, 0L, 0L))%>%
    dplyr::group_by(cohort_definition_id_drugs, strata) %>%
    dplyr::summarize(nn = sum(hit, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(nn = as.double(nn),
                  cohort_id = as.integer(cohort_definition_id_drugs)) %>%
    dplyr::left_join(denom, by = c("strata")) %>%
    dplyr::select(-cohort_definition_id_drugs) %>%
    dplyr::mutate(pct_raw = nn / tot) %>%
    dplyr::arrange(cohort_id, strata) %>%
    dplyr::collect() %>%
    dplyr::right_join(cohort_key, by = c("cohort_id")) %>%
    dplyr::mutate(
      domain = "Cohorts",
      chapter_name = "Drugs",
      group_name = "Drugs"
    ) %>%
    dplyr::select(domain, chapter_name, group_name,
                  cohort_name, cohort_id,
                  strata, nn, pct_raw)

  tok <- Sys.time() - tik
  cli::cat_line(crayon::yellow(glue::glue("   Process took {round(tok, digits = 2)} seconds")))
  #setup save path
  save_name <- paste("post_index_drug", startDays, endDays, sep = "_")
  save_path <- file.path(output_path, save_name)

  #save post index characterization
  arrow::write_feather(post_index_tbl, sink = save_path)
  usethis::ui_info("Post Index Drugs for window ({startDays}, {endDays}) written as a feather file to {ui_field(save_path)}")

  invisible(post_index_tbl)

}


#' Function that gets the post_index drugs for the analysis
#'
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
#' @include helpers.R
#' @export
generate_post_index_drugs <- function(execution_settings,
                                      target_cohort_id,
                                      target_cohort_name,
                                      cohort_meta,
                                      strata_id,
                                      strata_name,
                                      output_path) {


  ## Set up save path -------------------------------

  #create the file path string
  output_path <- file.path(output_path,
                           execution_settings$databaseId,
                           target_cohort_name,
                           strata_name)

  # create directory if it does not exist
  if(!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }


  #create post index analysis ---------------------------------
  ParallelLogger::logInfo(
    "- Generating Post Index Drug Characteristics\n\t",
    "> Cohort ",target_cohort_id, ": ", crayon::yellow(target_cohort_name),
    "\n\t",
    "> Strata ", strata_id, ": ", crayon::yellow(strata_name)
  )

  startDays <- c(0, 91)
  endDays <- c(90, 365)

  #get the ids of the cohort definitions used as drugs for post-index
  df <- cohort_meta %>%
    filter(grepl("drugs", cohort_file)) %>%
    select(cohort_id, cohort_name_short)

  #create post index drug cohorts

  purrr::walk2(startDays, endDays,
               ~post_index_drug_cohorts(
                 execution_settings = execution_settings,
                 target_cohort_id = target_cohort_id,
                 strata_id = strata_id,
                 drug_cohort_ids = df$cohort_id,
                 drug_cohort_names = df$cohort_name_short,
                 startDays = .x,
                 endDays = .y,
                 output_path = output_path
               )
  )

  invisible(df)

}

#' Function to run post index drug analysis
#'
#' @param execution_settings executionSettings object with connection credentials
#' @param cohort_meta file that specifies all cohorts created for the study.
#' @param strata_meta a tibble generated from build_boris_strata
#' @export
run_post_index_drug_analysis <- function(execution_settings,
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
               ~generate_post_index_drugs(
                 execution_settings = execution_settings,
                 target_cohort_id = ..1,
                 target_cohort_name = ..2,
                 strata_id = ..3,
                 strata_name = ..4,
                 cohort_meta = cohort_meta,
                 output_path = "output/post_index"
               ))

  invisible(exposure_cohorts)

}
