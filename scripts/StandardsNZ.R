StandardsNZ_read_file <- function(x) {
  read_csv(x, col_types = cols(.default = "c")) %>% 
    distinct() %>% 
    select(date = 3)
}

StandardsNZ_Prep <- function(x) {
  x %>%
    mutate(reporting_period = dmy(date) %>% floor_date("month")) %>% 
    count(reporting_period, name = "value") %>%
    mutate(metric_name = "views", database = "Standards Online New Zealand", publisher = "Standards New Zealand")
}

# Bind the data frames together, deduplicate the rows, and run through the preparatory function
directory_path = "data/raw/StandardsNZ"
StandardsNZ <- lapply(paste(directory_path, list.files(directory_path), sep="/"), StandardsNZ_read_file) %>%
  bind_rows() %>% 
  distinct() %>% 
  StandardsNZ_Prep
