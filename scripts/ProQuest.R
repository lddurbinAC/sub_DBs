recent_databases <- read.csv("data/ProQuest_db_list.csv", header = T)
research_libraries <- c("Research Library: Business|Research Library: Health & Medicine|Research Library: History|Research Library: Literature & Language|Research Library: Science & Technology|Research Library: Social Sciences|Research Library: The Arts")

# Verify assumptions about the data using assertr
check_recent_file_structure <- . %>%
  chain_start %>%
  verify(ncol(.) > 5) %>% 
  verify(str_to_title(str_sub(colnames(.), 1, 3)[-c(1:7)]) %in% months) %>% 
  verify(ncol(select_if(., is.character)) == 5) %>% 
  chain_end

# Take the historic data and prepare it for transformation into the standardised output
ProQuestHistoricPrep <- function(x) {
  df <- read_tsv(x, skip=7, col_names=T, col_types = cols(.default = "?")) %>%
    select("database" = 1, "publisher" = 2, "metric_name" = 4, everything(), -c(3,5))

  #SciTech Premium Collection is the average of all SciTech Premium databases, which return the same number. 
  SciTech <- filter(df, str_detect(database, "SciTech Premium")) %>%
    mutate(database = "SciTech Premium Collection") %>%
    group_by(database, publisher, metric_name) %>%
    summarise_all(mean, .groups = "drop") %>%
    mutate(across(where(is.numeric), round))

  df %>%filter(!str_detect(database, "SciTech Premium")) %>%
    bind_rows(., SciTech) %>%
    ProQuestPrep()
}

# Take the recent data and prepare it for transformation into the standardised output
ProQuestRecentPrep <- function(x) {
  df <- read_tsv(x, skip=13, col_names=T, col_types = cols(.default = "?")) %>%
    check_recent_file_structure
  
  df %>% select("database" = 1, "publisher" = 2, "metric_name" = 6, everything(), -c(3:5, 7)) %>%
    ProQuestPrep()
}

# Take the raw data (either recent or historic) and transform it into the standardised output
ProQuestPrep <- function(ProQuest) {
  ProQuest %>% clean_and_gather(4:ncol(.), "reporting_period") %>% 
    filter(metric_name %in% c("Regular Searches", "Searches_Regular", "Total_Item_Requests") & (database %in% recent_databases$Database | str_detect(database, "Research Library"))) %>% 
    mutate(database = str_replace_all(database, research_libraries, "ProQuest Research Library"), reporting_period = as.Date(paste("01", reporting_period, sep="_"), format="%d_%b_%Y"), metric_name = case_when(
      metric_name == "Regular Searches" ~ "Searches",
      metric_name == "Searches_Regular" ~ "Searches",
      metric_name == "Total_Item_Requests" ~ "Views"
    )) %>% 
    group_by_at(vars(-value)) %>% 
    summarise(value = sum(value), .groups = "drop")
}

# Run the raw data files through the preparatory function and bind the data frames together
ProQuest_historic <- process_files("data/raw/ProQuest/historic", ProQuestHistoricPrep)
ProQuest_recent <- process_files("data/raw/ProQuest", ProQuestRecentPrep)

ProQuest <- bind_rows(ProQuest_historic, ProQuest_recent)
