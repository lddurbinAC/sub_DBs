# Read CSV, clean column names, select required fields
PressReader_prep <- function(csv_file) {
  read_csv(csv_file, col_type = cols(), skip = 7) %>% 
    clean_names() %>% 
    select(date, article_opens, sessions)
}

files <- list.files("data/raw/PressReader_new", pattern = "*.csv$", full.names = TRUE)

PressReader_new <- lapply(files, PressReader_prep) %>% 
  bind_rows() %>% 
  group_by(reporting_period = floor_date(date, "month")) %>% 
  summarise(article_opens = sum(article_opens), sessions = sum(sessions), .groups = "drop") %>% 
  pivot_longer(c(article_opens, sessions), names_to = "metric_name") %>% 
  mutate(database = "PressReader (Connections)", publisher = "PressReader", metric_name = case_when(
    metric_name == "sessions" ~ "Sessions",
    metric_name == "article_opens" ~ "Views"
  )) %>% 
  filter(reporting_period > as.Date("2021-06-01"))
