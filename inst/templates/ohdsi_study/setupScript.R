library(picard)

edit_config()
database <- "<insert_database>"
set_credentials(database)
check_credentials(database)


initialize_cohort_tables(database)
