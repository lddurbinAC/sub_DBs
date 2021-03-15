# Verify assumptions about the data using assertr
check_file_structure <- . %>%
  chain_start %>%
  verify(ncol(.) == 15) %>% 
  verify(has_all_names("Month.Year", "Logins", "Total.views")) %>%
  verify(ncol(select_if(., is.numeric)) == 14) %>% 
  chain_end

# Take the raw data and transform it into the standardised output
LyndaPrep <- function(x) {
  df <- read.csv(x, stringsAsFactors = F, header=T, skip=2) %>% 
    check_file_structure
  
  df %>% mutate(reporting_period = as.Date(paste("01", Month.Year, sep = "/"), format = "%d/%m/%y")) %>% 
    select(reporting_period, 7, 10) %>% 
    clean_and_gather(2:3, "metric_name") %>%
    mutate(database = "Lynda.com", publisher = "Lynda.com", metric_name = case_when(
      metric_name == "logins" ~ "Sessions",
      metric_name == "total_views" ~ "Views"
    ))
}

# Run the raw data files through the preparatory function and bind the data frames together
Lynda <- process_files("data/raw/Lynda", LyndaPrep)
