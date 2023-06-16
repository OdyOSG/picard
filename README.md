# picard

<!-- badges: start -->

<!-- badges: end -->

**NOTE**: picard has been renamed Ulysses and its main repository will be moved to ohdsi github in due course. The functionality described in picard is the same underlying code for Ulysses. 

picard is a workflow package that helps initialize and facilitate an OHDSI project by automating start-up tasks and setting basic organization. It draws inspiration from [`usethis`](https://usethis.r-lib.org/) but designed specifically for OHDSI projects.

## Installation

You can install the development version of picard from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("OdyOSG/picard")
```

## How To Use

### Getting Started

`picard` is used to initialize an OHDSI Study project. This initiates a specific directory structure:

- analysis: folder containing analysis scripts and strategus json
- cohortsToCreate: folder containing cohort jsons used in the analysis
- results: folder to store results from analysis
- logs: folder to store logs
- extras: folder to store additional files
- _study.yml: a meta file describing details about the study
- .RProj

This consistent structure is intended to enforce better organization and a recognizable setup for study nodes to follow.

To start an OHDSI study project using `picard` one can follow the example code below:

``` r

library(picard)

newOhdsiStudy(projectName = "demo", author = "John Smith", type = "Characterization")

```

### File Templates

For those familiar with `usethis`, an R developer can quickly create files that are needed in the development pipeline for a package. Using this same idea in the context of OHDSI, `picard` automates files that are commonly used in the development of an ohdsi study. Once an OHDSI study has started, the user can add files that are commonly used in an OHDSI study such as:

- README.md: used to describe the intent of the study, a cover-page for the repo
- NEWS.md: used to show changes over time in the code
- Protocol: used to provide directions on the scientific methods of the study


`picard` has also introduced new files that may be helpful to a user developing an OHDSI study:

- AnalysisScript.R: a formatted R script that has connection details pre-rendered and ready for code development
- KeyringSetup.R: a set of code to provide a setup guide to using keyring
- CohortDetails.md: a tracker file used in tandem with the json that provides a layman explanation of cohort definitions and changes over time 
- StudySynopsis.qmd: a simplification of the Protocol to use in less formal settings
- ExampleScript.R: a pre-rendered file that can be used to provide reproducible OHDSI code examples
- config.yml: used to store DB connection information
- HowToRun.md: used to describe how to install and run the study

New file templates will be added over time. 


### Tools for a Study Node

Another use-case for `picard` is providing tools to a study node to help run a network study. For example, if a study node wants to participate in a study they can download the repo to their local computer using the following command:

```
picard::downloadStudy(org = "ohdsi-studies", repo = "AntiVegfKidneyFailure", 
                      savePath = "<path_to_study>")
```

This function will download the repo contents at place them in a folder accessible via R. The study node can now follow the instructions for running the study with the local copy.

Additionally, `picard` had created automated messages that can be used to help request for participation to the study host. Eventually, we will add support to post this message directly to the OHDSI Forums. 

### Adding Cohorts to the Study

From experience, cohorts are difficult to keep organized. `picard` attempts to organize cohort json in the cohortsToCreate folder making it easier to communicate which json was used in the analysis and target files to build cohorts via `CohortGenerator`. Examples of these functions will be provided in time. 
