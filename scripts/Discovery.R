# Run the raw data files through the preparatory function and bind the data frames together. Filter out data already captured in HistoricSubDBs
Discovery <- process_files("data/raw/Discovery", digitales_dashboard) %>% 
  filter(reporting_period > "2020-02-01") %>% 
  distinct(reporting_period, .keep_all = TRUE)
