######################################################################
# Example generic inverse variance meta-analysis
# Will Patterson
# Based on work by Adrian V. Hernandez
######################################################################

# packages
library(meta)
library(metafor)
library(readxl)
library(tidyverse)

# paths
file_path <- "data/raw.xlsx"
keyv_path <- "data/study_key.csv"

# data import
raw_data <- excel_sheets(file_path) %>%
  setNames(., .) %>%
  map(~ read_excel(file_path, sheet = .x, 
                   range = cell_cols("A:J"))) %>% # don't need verified col
  reduce(bind_rows) %>%                           # reduce to 1 object
  janitor::clean_names() %>%                      # make var names accessible
  merge(., read_csv(keyv_path),                   # merge key-value pairs
        by.x="study", by.y="title") 

# what risk factors occur more than n times?
inclusion_thresh <- 3

# list risk factors
risk_factors <- raw_data %>%
  group_by(risk_factor) %>%
  summarise(count = n()) %>%
  filter(count >= inclusion_thresh)

# for example, look at PPIs and risk of HA-CDI
ppi <- raw_data %>%
  filter(risk_factor == "PPI") %>% # NB: can be replaced with any risk_factor
  metagen(TE = log_or, seTE = se_log_or, 
          studlab = study_id, 
          sm = "OR", method.tau = "PM", 
          fixed = F)

# example forest plot
ppi %>% forest(layout = "RevMan5") 
