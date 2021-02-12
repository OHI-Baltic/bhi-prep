## Libraries ----
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

## Directories ----
## CHANGE BHI PREP DIRECTORY HERE!
dir_prep <- here::here()
dir_layers <- file.path(dir_prep, "layers")
dir_assess <- stringr::str_replace(dir_prep, pattern = "bhi-prep$", "bhi")

# CHANGE MAIN AUX BHI DIRECTORY HERE!
dir_B <- file.path(dirname(dir_prep), "bhi-data", "BHI 3.0")
if(!file.exists(dir_B)){ 
  ## warning if BHI internal, shared directory doesn't exist
  sprintf(
    "The BHI directory dir_share set in R/setup.R does not exist.\n
    Do you need to mount the BHI server: %s?", 
    dir_B
  )
}

## Global ----
assess_year <- 2021 # CHANGE BHI ASSESSMENT YEAR HERE!
version_year <- paste0("v", assess_year)