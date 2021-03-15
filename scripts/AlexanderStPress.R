# List the Alexander St Press Press databases to be included in reporting
databases <- read.csv("data/AlexanderStPress_db_list.csv", header = T)

# List the databases that will be re-named as "Music and Performing Arts Channel"
MusicAndPerformingArtsChannel <- c("African American Music Reference|American Song|Classical Music in Video|Classical Music Library|Classical Music Reference Library|Classical Scores Library, Volume 1|Classical Scores Library, Volume 2|Classical Scores Library, Volume 3|Classical Scores Library, Volume 4|Contemporary World Music|Dance Online: Dance in Video, Volume 1|Dance Online: Dance in Video, Volume 2|Jazz Music Library|Popular Music Library|Smithsonian Global Sound for Libraries")

# Verify assumptions about the data using assertr
check_file_structure <- . %>%
  chain_start %>%
  verify(ncol(.) >= 6) %>% 
  verify(has_all_names("Database", "Publisher", "Platform", "User.Activity")) %>%
  verify(str_to_title(str_sub(colnames(.), 1, 3)[-c(1:5)]) %in% months) %>% 
  assert(in_set("Record Views", "Regular Searches", "Result Clicks", "Searches-federated and automated"), User.Activity) %>% 
  chain_end

# Take the raw data and transform it into the standardised output
AlexanderStPressPrep <- function(x) {
  df = read.csv(x, skip = 7, header = T, stringsAsFactors=F) %>% 
    check_file_structure
  
  df %>% mutate(Database = str_replace_all(Database, MusicAndPerformingArtsChannel, "Music and Performing Arts Channel")) %>% 
    filter(Database %in% databases$Database & User.Activity %in% c("Record Views", "Regular Searches")) %>% 
    select(everything(), metric_name = User.Activity, -3, -5) %>% 
    clean_and_gather(4:(ncol(.)), "reporting_period") %>% 
    mutate(reporting_period = as.Date(paste("01", reporting_period, sep="_"), format="%d_%b_%Y"), metric_name = case_when(
      metric_name == "Regular Searches" ~ "Searches",
      metric_name == "Record Views" ~ "Views"
    )) %>% 
    group_by_at(vars(-value)) %>% 
    summarise(value = sum(value), .groups = "drop")
}

# Run the raw data files through the preparatory function and bind the data frames together
AlexanderStPress <- process_files("data/raw/AlexanderStPress", AlexanderStPressPrep)
