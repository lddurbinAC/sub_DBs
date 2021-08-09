# List the EBDCO databases to be included in reporting
databases <- read.csv("data/EBSCO_db_list.csv", header = T)

# Verify assumptions about the data using assertr
# check_file_structure <- . %>%
#   chain_start %>%
#   verify(ncol(.) > 15) %>% 
#   verify(has_all_names("Database", "Year", "Month", "Database Sessions", "Total Searches", "Total Requests")) %>%
#   verify(databases$Database %in% Database) %>% 
#   verify(ncol(select_if(., is.numeric)) > 14) %>% 
#   chain_end

# Take the raw data and transform it into the standardised output
EBSCO_Prep <- function(x) {
  df <- read_excel(x, skip = 7, col_names = T) 
  
  df %>% select(c(1, 4:8)) %>%
    clean_and_gather(4:6, "metric_name") %>%
    mutate(reporting_period = as.Date(paste("01", month, year, sep="/"), "%d/%B/%Y"), publisher = "EBSCO", metric_name = case_when(
      metric_name == "database_sessions" ~ "Sessions",
      metric_name == "total_searches" ~ "Searches",
      metric_name == "total_requests" ~ "Views"
    )) %>%
    select(-c("year", "month")) %>%
    filter(database %in% databases$Database)
}

# Run the raw data files through the preparatory function and bind the data frames together
EBSCO <- process_files("data/raw/EBSCO", EBSCO_Prep)
