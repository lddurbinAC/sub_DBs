PressReader_read_file <- function(x) {
  read.csv(x, stringsAsFactors = F, header=T) %>% 
    select(Date = 1, everything()) %>% 
    mutate_all(~(as.character(.)))
}

PressReader_Prep <- function(sessions, views) {
  rbind(sessions %>% select(Date, metric_name, value1 = 2, value2 = 3), views %>% select(Date, metric_name, value1 = 2, value2 = 3)) %>% 
    mutate(
      value1 = as.numeric(gsub(",", "", value1)),
      value2 = as.numeric(gsub(",", "", value2)),
      month = format(as.Date(Date, "%m/%d/%Y %H:%M:%S"), "%b"),
      year = format(as.Date(Date, "%m/%d/%Y %H:%M:%S"), "%Y"),
      database = "PressReader (Hub)",
      publisher = "PressReader"
      ) %>% 
    distinct() %>% 
    rowwise() %>% 
    mutate(value = sum(value1, value2, na.rm = T)) %>% 
    select(database, publisher, month, year, metric_name, value) %>% 
    group_by_at(vars(-value)) %>% 
    summarise(value = sum(value), .groups = "drop") %>% 
    mutate(reporting_period = as.Date(paste("01", month, year, sep = "/"), format = "%d/%b/%Y")) %>% 
    select(-month, -year)
}

# Views (a.k.a. Artticles Read) - onsite and offiste
PressReader_offsite.views <- process_files("data/raw/PressReader/offsite/views", PressReader_read_file)
PressReader_onsite.views <- process_files("data/raw/PressReader/onsite/views", PressReader_read_file)
PressReader.views <- merge(x = PressReader_onsite.views, y = PressReader_offsite.views, by="Date", all = T) %>% 
  mutate(metric_name = "Views")

# Sessions (a.k.a. HotSpot Connections) - onsite and offsite
PressReader_offsite.sessions <- process_files("data/raw/PressReader/offsite/sessions", PressReader_read_file)
PressReader_onsite.sessions <- process_files("data/raw/PressReader/onsite/sessions", PressReader_read_file)
PressReader.sessions <- merge(x = PressReader_onsite.sessions, y = PressReader_offsite.sessions, by="Date", all = T) %>% 
  mutate(metric_name = "Sessions")

# Bind and prep sessions and views data
PressReader <- PressReader_Prep(PressReader.sessions, PressReader.views)
