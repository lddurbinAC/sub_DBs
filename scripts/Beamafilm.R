# Run the raw data files through the preparatory function and bind the data frames together. Filter out data already captured in HistoricSubDBs
Beamafilm <- process_files("data/raw/Beamafilm", digitales_dashboard) %>% 
  filter(reporting_period > "2020-07-01") %>% 
  distinct(reporting_period, .keep_all = TRUE)
