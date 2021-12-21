library(reticulate)
library(here)
library(tidyverse)
library(nycflights13)
library(ggplot2)
library(recipes)   # for data preprocessing


# ==========================================================================
# Example Usage with Single Continuous Treatment Observational Data
# ==========================================================================
# 


econml <- import("econml")
sklearn <- import("sklearn")

#'-----------------------
#' Data 
#'-----------------------
oj_raw <- oj_raw <- read_csv("https://msalicedatapublic.blob.core.windows.net/datasets/OrangeJuice/oj_large.csv")

names(oj_raw)
#'-----------------------
#' Preprocessing 
#'-----------------------
# logmove: 

oj_rec <- recipe(logmove ~ ., data = oj_raw) %>% 
# normalize numeric data to have a standard deviation of one and a mean of zero#
  step_normalize(
    INCOME, AGE60, EDUC, ETHNIC, INCOME,
    HHLARGE, WORKWOM, HVAL150, SSTRDIST,
    SSTRVOL, CPDIST5, CPWVOL5
  ) %>% 
# convert nominal data (e.g. character or factors) into one or more numeric binary model
  step_dummy(brand, one_hot = TRUE) %>% 
  step_log(price) %>% 
  prep() %>% 
# Extract transformed training set
  juice()

head(oj_rec)


Y <- oj_rec %>%
# This works like [[ for local data frames
  pull(logmove) %>%
# coerced to an `array`
  as.array() %>% 
# remove the `names` or `dummies` attributes of an R object
  unname()
  
D <- oj_rec %>%
  pull(price) %>%
  as.array() %>% 
  unname()

X <- oj_rec %>%
  select(INCOME) %>%
  as.matrix() %>% 
  unname()

W <- oj_rec %>%
  select(AGE60:CPWVOL5, starts_with("brand")) %>%
  as.matrix() %>% 
  unname()


#### == Genearate test data == ####
min_income <- -1
max_income <- 1
delta <- (max_income - min_income) / 100

X_test <- seq(max_income, min_income, -delta) %>%
  as.matrix()


#'-----------------------
#' Estimate CATE
#'-----------------------
repl_python()

















