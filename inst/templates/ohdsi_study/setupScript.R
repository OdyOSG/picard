library(picard)

edit_config()
config_block <- "<insert_database>"
set_credentials(config_block)
check_credentials(config_block)


initialize_cohort_tables(config_block)
