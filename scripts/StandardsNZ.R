StandardsNZ_read_file <- function(x) {
  read.csv(x, stringsAsFactors = F, header=T)
}

StandardsNZ_Prep <- function(x) {
  x %>% distinct() %>%
    select(date = 3) %>% 
    mutate(reporting_period = dmy(date)) %>% 
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
  StandardsNZ_Prep
