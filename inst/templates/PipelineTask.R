# A. Meta Info -----------------------

# Task: {{{ Task }}}
# Author: {{{ Author }}}
# Date: {{{ Date }}}
# Description: The purpose of {{{ FileName }}}.R is to.....


# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)

# C. Variables -----------------------

## Admin variables -------------------

outputFolder <- here::here("output/{{{ FileName }}}") %>%
  fs::dir_create()

## Study Variables -------------------

# Add variables for study here

# D. Connection ----------------------

# add connection code here


# E. Script --------------------

# Add script here

# F. Session Info ------------------------

sessioninfo::session_info()

