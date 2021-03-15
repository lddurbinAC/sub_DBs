# List the OUP databases to be included in reporting
databases <- read.csv("data/OUP_db_list.csv", header = T)

# Select relevant columns and rows from post-June 2018 data
OUP_file_prep <- function(x) {
  x %>% read.csv(stringsAsFactors = F, header=T, skip=13) %>%
    select(database = 1, metric_name = 2, everything(), -c(3, ncol(.))) %>% 
    OUP_Prep()
}

# Verify assumptions about the data using assertr
check_file_structure <- . %>%
  chain_start %>%
  verify(ncol(.) > 2) %>% 
  verify(str_to_title(str_sub(colnames(.), 1, 3)[-c(1:2)]) %in% months) %>% 
  verify(ncol(select_if(., is.character)) == 2) %>% 
  chain_end

# Take the raw data and transform it into the standardised output
OUP_Prep <- function(x) {
  x %>% check_file_structure() %>% 
    clean_and_gather(3:(ncol(.)), "reporting_period") %>% 
    filter(database %in% databases$Database & metric_name %in% c("Regular Searches", "Searches_Platform", "Total_Item_Requests")) %>% 
    mutate(reporting_period = as.Date(paste("01", reporting_period, sep = "_"), format = "%d_%b_%Y"), publisher = "Oxford University Press", metric_name = case_when(
      metric_name == "Regular Searches" ~ "Searches", #searches before July 2018
      metric_name == "Searches_Platform" ~ "Searches", #searches from July 2018
      metric_name == "Total_Item_Requests" ~ "Views" #views from July 2018
    )) %>% 
    group_by_at(vars(-value)) %>% 
    summarise(value = sum(value), .groups = "drop")
}

# Select relevant columns and rows from pre-July 2018 data
OUP_201501_201806 <- read.csv("data/raw/OUP/historic/OUP_Platform_201501-201806.csv", stringsAsFactors = F, header=T, skip=7) %>%
  select(database = 1, metric_name = 3, everything(), -c(2, 4, ncol(.)))

# Run the raw data files through the preparatory function
OUP <- rbind(OUP_Prep(OUP_201501_201806), process_files("data/raw/OUP", OUP_file_prep))
