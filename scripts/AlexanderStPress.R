# Take the raw data and transform it into the standardised output
AlexanderStPressPrep <- function(x) {
  read_csv(x, col_types = "ccccdd", skip = 7, name_repair = make_clean_names) %>% 
    mutate(database = str_replace_all(database, MusicAndPerformingArtsChannel, "Music and Performing Arts Channel")) %>% 
    filter(database %in% databases$Database & user_activity %in% c("Record Views", "Regular Searches")) %>% 
    select(everything(), metric_name = user_activity, -3, -5) %>% 
    clean_and_gather(4:(ncol(.)), "reporting_period") %>% 
    mutate(
      reporting_period = as.Date(paste("01", reporting_period, sep="_"), format="%d_%b_%Y"),
      metric_name = case_when(
        metric_name == "Regular Searches" ~ "Searches",
        metric_name == "Record Views" ~ "Views"
        )) %>% 
    with_groups(c(-value), summarise, value = sum(value))
}

# List the Alexander St Press Press databases to be included in reporting
databases <- read_csv("data/AlexanderStPress_db_list.csv", col_types = "cc")

# List the databases that will be re-named as "Music and Performing Arts Channel"
MusicAndPerformingArtsChannel <- c("African American Music Reference|American Song|Classical Music in Video|Classical Music Library|Classical Music Reference Library|Classical Scores Library, Volume 1|Classical Scores Library, Volume 2|Classical Scores Library, Volume 3|Classical Scores Library, Volume 4|Contemporary World Music|Dance Online: Dance in Video, Volume 1|Dance Online: Dance in Video, Volume 2|Jazz Music Library|Popular Music Library|Smithsonian Global Sound for Libraries")

# Run the raw data files through the preparatory function and bind the data frames together
AlexanderStPress <- process_files("data/raw/AlexanderStPress", AlexanderStPressPrep)
