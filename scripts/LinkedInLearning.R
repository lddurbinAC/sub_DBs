LinkedInPrep <- function(x) {
  read_csv(x, col_types = "ccnnnnnnnnnnnnn") %>% 
    clean_names() %>% 
    mutate(reporting_period = mdy(start_day_pst_pdt) %>% floor_date("month"))
}

# Run the raw data files through the preparatory function and bind the data frames together
LinkedIn <- process_files("data/raw/LinkedInLearning", LinkedInPrep)

read_csv(list.files("data/raw/LinkedInLearning", full.names = TRUE), col_types = "ccnnnnnnnnnnnnn") %>% 
  clean_names() %>% 
  mutate(reporting_period = mdy(start_day_pst_pdt) %>% floor_date("month"))