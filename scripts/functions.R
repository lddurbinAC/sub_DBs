# Run multiple files through the relevant preparatory function and bind them together
process_files <- function(directory_path, function_name) {
  paste(directory_path, list.files(directory_path, pattern = "\\."), sep="/") %>%
    lapply(function_name) %>%
    bind_rows()
}

# Format values as numeric, gather data, rename values column
clean_and_gather <- function(df, values, keyName) {
  clean_names(df) %>%
    mutate_at(values, as.numeric) %>%
    pivot_longer(c(values), names_to = keyName, values_to = "value")
}

# Verify and prepare data downloaded from the Digitales dashboard
digitales_dashboard <- function(x) {
  # Verify assumptions about the data using assertr
  check_file_structure <- . %>%
    chain_start %>%
    verify(ncol(.) == 6) %>% 
    verify(has_all_names("Customer", "Resource", "Year", "Month", "Click", "Renewal Date")) %>%
    verify(ncol(select_if(., is.numeric)) == 1) %>% 
    chain_end
  
  # Take the raw data and transform it into the standardised output
  df <- read_csv(x, col_types = "ccccnc") %>% 
    check_file_structure
  df %>% mutate(reporting_period = as.Date(paste("01", Month, Year, sep="/"), "%d/%B/%Y"), metric_name = "Views", publisher = "Digitales") %>% 
    select(everything(), -c(1, 3:4, 6), value = Click, database = Resource)
}