# Welcome to OHDSI Project

You have successfully initialized an ohdsi project using the picard
package. This file provides guidance on how to configure the ohdsi study
for your OMOP data and add extensions for your analysis.

## Contents

An OHDSI Project contains 3 folders and 3 files at its root.

1)  WELCOME.MD - this file introudcing the ohdsi project

2)  config.yml - this file stores the database credential information.
    More details about this file will be discussed in the next section.

3)  codeToRun.R - this file contains code to run the analysis.

4)  input - this folder contains files needed to run the analysis. It is
    pre-populated with a 'cohorts_to_create' folder that users use to
    load cohort definition jsons for the analysis. A 'cohort_updates.md'
    file is pre-initialized which is used to provide context of the
    cohort definition versioning. Other files that can be added to the
    input folder include csv's of concept sets or sql of cohorts. These
    can be added at the users discretion.

5)  output - this folder contains files that are output from the
    analysis. We pre-populated three folders for cohort_build, baseline
    and diagnostics as basic builders for all ohdsi analysis. A user can
    add folders for other output at their discretion.

6)  R - this folder is initialized to store any additional code used in
    the study

**Future addition**: An addition of an renv.lock and renv folder will be
added so users can restore the R session to run the analysis.

## Getting Started

### Introducing the config.yml file

To start the project we must first set up the 'config.yml' file to
access the OMOP databases. To edit the config.yml file run:
`picard::edit_config()`. This will open the file (example below).


```
# Startup config.yml file

default:
  studyName: ohdsi_study
  cohortTableName: cohorts

# a dummy block is added as a guide
example:
  databaseName: example
  connectionDetails: !expr DatabaseConnector::createConnectionDetails(
    dbms = keyring::key_get("example_dbms"),
    user = keyring::key_get("example_user"),
    password = keyring::key_get("example_password"),
    server = keyring::key_get("example_server"),
    port = keyring::key_get('example_port'))
  cdm: !expr keyring::key_get("example_cdm")
  vocab: !expr keyring::key_get("example_vocab")
  write: !expr keyring::key_get("example_write")

```

The config file is used to access credentials through the 
`picard::execution_settings(database)` function. The config file must maintain
a default tab. Under the default the user should change the study name and the 
cohort table names to something descriptive for the study. Next the user needs to change the second tab to a name for the database used. For example I would replace 'example' with 'synpuf'.
under the databaseName slot the use should place the name of the database as it is refered to in the dbms. For example I would replace 'example' with 'synpuf_110k'. Another thing to notice in the config file is that the connectionDetails, cdm, vocab, and write slots are followed by a tag `!expr`. This indicates the evaluation of an R command from the config file. When the config.yml file is accessed it will evaluate the function on the fly. We keep this in place to execute the `createConnectionDetails` command and the `key_get` commands. It is suggested that the user uses the `keyring` package to store all their credentials. The user may replace the code with text strings if they wish, as shown in the example below:


```
# Startup config.yml file

default:
  studyName: my_study
  cohortTableName: my_cohorts

# a dummy block is added as a guide
synpuf:
  databaseName: synpuf_110k
  connectionDetails: !expr DatabaseConnector::createConnectionDetails(
    dbms = "dbms",
    user = "user",
    password = "password",
    server = "server",
    port = 'port')
  cdm: cdm
  vocab: vocab
  write: write

```

### Setting Credentials

Once we have made these changes we can close the config.yml file and restart the R session. Next, the user is to update the keyring api with their passwords. To do this run `picard::set_credentials(database)`. When this is run it will prompt you to set the key for each credential in the config block. You can check the credentials when you are finished using: `picard::check_credentials(database)`. 


### Initializing cohort tables

Once you have set up the config file you now can initialize the cohort tables. Run function `picard::initialize_cohort_tables(database)` to set these tables. 

## Starting the study

Once the config file has been set and the cohort tables have been initialized we proceed with preparing the study to be run:

1) Load json files into 'input/cohorts_to_create'. Separate into sensible folders. 
2) Annotate 'input/cohort_updated.md'
3) Add custom functions in the R folder and include into codeToRun.R
4) Engage codeToRun.R

