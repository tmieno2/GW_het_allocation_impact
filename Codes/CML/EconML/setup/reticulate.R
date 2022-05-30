library(reticulate)
library(here)
library(tidyverse)
library(nycflights13)
library(ggplot2)
library(recipes)   # for data preprocessing

# ==========================================================================
# Example
# ==========================================================================

####==== getting started ====####

#- which python? -#
Sys.which("python")

#- system configuration -#
py_config()

####==== Sourcing python script and run the python function ====####
data(flights)
source_python(here("GitControlled", "GW_het_allocation_impact", "Codes", "EconML", "flights.py"))
out_flights <- read_flights(flights)


ggplot(out_flights, aes(carrier, arr_delay)) + geom_point() + geom_jitter()

####==== install python package ====####
#- let's see all available cond environments -#
conda_list()

#- install python package within Conda environment named “r-reticulate”.-#
# conda_install("r-reticulate", "scipy")

#- when calling package -#
# use_virtualenv("r-reticulate")
# scipy <- import("scipy")



#### ====  work Python interactively on Rstudio ==== ####
repl_python()































