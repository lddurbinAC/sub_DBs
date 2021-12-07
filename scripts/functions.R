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
  # Take the raw data and transform it into the standardised output
  df <- read_excel(x, col_types = "ccccnc")
  
  df %>% mutate(reporting_period = as.Date(paste("01", Month, Year, sep="/"), "%d/%B/%Y"), metric_name = "Views", publisher = "Digitales") %>% 
    select(everything(), -c(1, 3:4, 6), value = Click, database = Resource)
}