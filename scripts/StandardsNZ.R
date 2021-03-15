# Verify assumptions about the data using assertr
check_file_structure <- . %>%
  chain_start %>%
  verify(ncol(.) == 4) %>% 
  verify(has_all_names("Product", "User", "Date", "Time")) %>%
  chain_end

StandardsNZ_read_file <- function(x) {
  read.csv(x, stringsAsFactors = F, header=T)
}

StandardsNZ_Prep <- function(x) {
  x %>% distinct() %>%
    clean_names() %>% 
    mutate(reporting_period = format(as.Date(date, format="%d-%m-%Y"), "%Y-%m-01")) %>%
    group_by(reporting_period) %>%
    mutate(id = n_distinct(reporting_period), value = sum(id), metric_name = "views", database = "Standards Online New Zealand", publisher = "Standards New Zealand") %>% 
    distinct(reporting_period, metric_name, value = as.double(value), database, publisher) %>% 
    ungroup() %>% 
    mutate(reporting_period = as.Date(reporting_period, format="%Y-%m-%d")) 
}

# Bind the data frames together, deduplicate the rows, and run through the preparatory function
directory_path = "data/raw/StandardsNZ"
StandardsNZ <- lapply(paste(directory_path, list.files(directory_path), sep="/"), StandardsNZ_read_file) %>%
  bind_rows() %>% 
  distinct() %>% 
  check_file_structure %>% 
  StandardsNZ_Prep
