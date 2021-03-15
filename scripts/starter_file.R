# *****************************************************************************
# Setup ----

# Load libraries
library(assertr)
library(tidyverse)
library(readxl)
library(janitor)
library(stringr)
library(lubridate)

# Abbreviated month names, used for checking column names
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Commonly-used functions
source("scripts/functions.R")

# *****************************************************************************


# *****************************************************************************
# Load and prep data ---- 

#Historic data for subscription databases that are now being entered into Microsoft Forms
source("scripts/HistoricSubDBs.R")

#Alexander St Press
source("scripts/AlexanderStPress.R")

#EBSCO Research Databases
source("scripts/EBSCO.R")

#Gale Cengage Databases
source("scripts/GaleDBs.R")

#Lynda.com
source("scripts/Lynda.R")

#LinkedIn Learning
#source("scripts/LinkedInLearning.R")

#Oxford University Press
source("scripts/OUP.R")

#ProQuest
source("scripts/ProQuest.R")

#JSTOR
source("scripts/JSTOR.R")

#PressReader
source("scripts/PressReader.R")

#StandardsNZ
source("scripts/StandardsNZ.R")

#Naxos Music Library and Naxos Jazz
source("scripts/Naxos.R")

#Beamafilm
source("scripts/Beamafilm.R")

#Discovery
source("scripts/Discovery.R")

#Subcription database data entered manually into Microsoft Forms
source("scripts/OtherSubDBs.R")

# *****************************************************************************


# *****************************************************************************
# Finishing touches ---- 

database_IDs <- read_csv("data/SubDBs_IDs.csv", col_types = "ccc")
databases_standardised <- read_csv("data/standardised_database_names.csv", col_types = "ccn") %>% select(-3)

# Bring all datasets together, re-order columns, add Month and Year, filter out this month's data, remove NAs
DatabaseSubs <- bind_rows(HistoricSubDBs, AlexanderStPress, EBSCO, GaleDBs, Lynda, OUP, JSTOR, PressReader, ProQuest, StandardsNZ, Naxos, OtherSubs, Discovery, Beamafilm) %>% 
  select(publisher, database, reporting_period, metric_name, value) %>% 
  mutate(metric_name = str_to_title(metric_name), Month = format(reporting_period, "%b"), Year = format(reporting_period, "%Y")) %>% 
  filter(reporting_period != as.Date(format(as.Date(format(Sys.Date(), "%Y-%m-01")), "%Y-%m-01")))

# # assigning unique IDs for each database
# DatabaseSubs %>%
#   left_join(databases_standardised, by = c("database" = "database_original")) %>% 
#   select(-database, database = database_amended) %>% 
#   distinct(across(publisher:database)) %>% 
#   group_by(database) %>%
#   mutate(ID = paste0("SubDB_", cur_group_id()+142)) %>%
#   ungroup() %>% 
#   distinct(database, publisher, ID) %>% 
#   write_csv("data/SubDBs_IDs.csv")

# Display any active databases missing from this month's data
# end of previous month:
eopm <- Sys.Date() - months(1) - days(day(Sys.Date()))
# start of previous month:
sopm <- Sys.Date() - days(day(Sys.Date()))
sopm <- sopm - days(day(sopm) - 1) - months(1)
previous_reporting_month <- interval(sopm, eopm)

expected_databases <- DatabaseSubs %>%
  filter(reporting_period %within% previous_reporting_month) %>% 
  distinct(database, publisher)

missing_DBs <- DatabaseSubs %>% filter(reporting_period == as.Date(format(as.Date(format(Sys.Date(), "%Y-%m-01"))-1, "%Y-%m-01"))) %>%
  verify(expected_databases$database %in% database, error_fun = error_df_return) %>% 
  select(index)
if (count(missing_DBs) > 0) {
  slice(expected_databases, missing_DBs$index) %>% View()
}

#get only subscription databases with sessions data for the All Visits report, save to csv
# DatabaseSubs %>%
#   filter(metric_name == "Sessions") %>%
#   select(database, Sessions = value, Month, Year) %>%
#   write.csv("data/processed/AllVisits_SubDBs.csv")

SubDBs_performance <- DatabaseSubs %>% 
  left_join(databases_standardised, by = c("database" = "database_original")) %>%
  select(-database, database = database_amended) %>%
  left_join(database_IDs, by = c("database", "publisher")) %>% 
  mutate(across(where(is.character), replace_na, replace = ""), across(where(is.numeric), replace_na, replace = 0))

# write_csv(SubDBs_performance, "data/processed/SubDBs_performance.csv")

# SubDBs_performance %>% select(Vendor = publisher, Title = database, ID) %>% distinct() %>% write_csv("data/processed/SubDBs_information.csv")


# *****************************************************************************
