library(knitr)
library(here)
library(reticulate)
library(tidyverse) # for data wrangling and visualization
library(recipes)   # for data preprocessing
library(data.table)

py_config()

# ==========================================================================
# DMLOrthoForest (suitable for continuous or discrete treatments)
# ==========================================================================

#### ==== load necessary modules ==== ####
source_python(here("GitControlled/EconML/Practice/ORF/setup.py"))

#### ==== Data Generation Process ==== ####
source_python(here("GitControlled/EconML/Practice/ORF/data.creation.py"))

#### ==== Run  ==== ####
# OrthoForest: 
# for continuous or discrete treatments using the DML residual on residual moment function.
# 
source_python(here("GitControlled/EconML/Practice/ORF/run_ORF.py"))

py$report

