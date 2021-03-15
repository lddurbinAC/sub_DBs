# Verify assumptions about the data using assertr
check_file_structure <- . %>%
  chain_start %>%
  verify(c("Total Numbers of Turnaways", "Total Logins (Including Mobile)") %in% X) %>% 
  chain_end

NaxosPrep <- function(x) {
  df <- read_excel(x, skip=8, .name_repair = ~ make.names(.x, unique = TRUE)) %>%
    select(-2) %>% 
    check_file_structure
  
  df %>% rename_all(~str_sub(paste(df[1,], substr(names(df), 2, 5), sep="_"), 1, 8)) %>%
    rename_at(1, ~"metric_name") %>% 
    slice(2:3) %>% 
    clean_and_gather(2:ncol(.), "reporting_period") %>% 
    mutate(publisher = "Triton AV", reporting_period = as.Date(paste("01", reporting_period, sep="_"), format="%d_%b_%Y"), metric_name = case_when(
      metric_name == "Total Numbers of Turnaways" ~ "Turnaways",
      metric_name == "Total Logins (Including Mobile)" ~ "Sessions"
      ))
}

NaxosMusicLibrary <- process_files("data/raw/Naxos/Naxos Music Library", NaxosPrep) %>% 
  mutate(database = "Naxos Music Library")

NaxosJazz <- process_files("data/raw/Naxos/Naxos Music Library - Jazz", NaxosPrep) %>% 
  mutate(database = "Naxos Music Library - Jazz")

Naxos <- bind_rows(NaxosMusicLibrary, NaxosJazz)
