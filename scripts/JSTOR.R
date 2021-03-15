# Verify assumptions about the data using assertr
check_file_structure <- . %>%
  chain_start %>%
  verify(ncol(.) > 7) %>% 
  verify(str_to_title(str_sub(colnames(.), 1, 3)[-c(1:7)]) %in% months) %>% 
  chain_end

JSTOR_Prep <- function(x, historic) {
  df <- if(historic) {
    read_excel(x, col_names = T, skip = 7) %>% 
      select(-c(2:3, 5))
  }  else {
    read_excel(x, col_names = T, skip = 13) %>%
      check_file_structure %>% 
      select(-c(2:5, 7))
  }
  df %>% clean_and_gather(3:ncol(df), "reporting_period") %>% 
    filter(.[[1]] == "JSTOR" & .[[2]] %in% c("Regular Searches", "Result Clicks", "Searches_Regular", "Total_Item_Requests")) %>% 
    mutate(publisher = "JSTOR", reporting_period = as.Date(paste("01", reporting_period, sep="_"), format="%d_%b_%Y"), metric_name = case_when(
        .[[2]] == "Regular Searches" ~ "Searches",
        .[[2]] == "Result Clicks" ~ "Views",
        .[[2]] == "Searches_Regular" ~ "Searches",
        .[[2]] == "Total_Item_Requests" ~ "Views"
        )
      ) %>% 
    select(-2)
}

# Run the historic (COUNTER4) raw data files through the preparatory function and bind the data frames together
file_path_historic <- "data/raw/JSTOR/historic"
JSTOR_historic <- lapply(paste(file_path_historic, list.files(file_path_historic), sep="/"), JSTOR_Prep, historic = T) %>% 
  bind_rows()

# Run the recent (COUNTER5) raw data files through the preparatory function and bind the data frames together
file_path_recent <- "data/raw/JSTOR"
JSTOR_recent <- lapply(paste(file_path_recent, str_subset(list.files(file_path_recent), ".xlsx"), sep="/"), JSTOR_Prep, historic = F) %>% 
  bind_rows()

JSTOR <- rbind(JSTOR_historic, JSTOR_recent)
