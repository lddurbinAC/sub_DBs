# List the Gale databases to be included in reporting
databases <- read.csv("data/Gale_db_list.csv")

# Verify assumptions about the data using assertr
check_file_structure <- . %>%
  chain_start %>%
  verify(ncol(.) >= 6) %>% 
  verify(ncol(select_if(., is.numeric)) >= 5) %>% 
  chain_end

# Take the raw data and transform it into the standardised output
GalePrep <- function(x) {
  df <- read.csv(x, stringsAsFactors = F, header=T) %>%
    check_file_structure
  
  df %>% select(database = 1, sessions = Sessions, searches = Searches, turnaways = ncol(.)) %>%
    clean_and_gather(2:4, "metric_name") %>%
    filter(database %in% databases$Database) %>%
    mutate(reporting_period = as.Date(paste("01", str_sub(basename(x), 6, 12)[1], sep=""), format="%d%m%Y"), publisher = "Gale Cengage", database = case_when(
      str_detect(database, "Communications and Mass") ~ "Gale OneFile: Communications and Mass Media",
      !is.na(database) ~ database)
    )
}

# Run the raw data files through the preparatory function and bind the data frames together
GaleDBs <- process_files("data/raw/Gale", GalePrep)
