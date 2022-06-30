# Read CSV, clean column names, select required fields
PressReader_prep <- function(csv_file) {
  skip_rows <- if_else(str_length(csv_file) > 100, 9, 7) # files from May 2022 are formatted differently
  
  read_csv(csv_file, col_type = cols(), skip = skip_rows) %>% 
    clean_names() %>% 
    select(date, article_opens, sessions)
}

# files <- fs::dir_ls("data/raw/PressReader_new")

PressReader_new <- lapply(fs::dir_ls("data/raw/PressReader_new"), PressReader_prep) %>% 
  bind_rows() %>% 
  group_by(reporting_period = floor_date(date, "month")) %>% 
  summarise(article_opens = sum(article_opens), sessions = sum(sessions), .groups = "drop") %>% 
  pivot_longer(c(article_opens, sessions), names_to = "metric_name") %>% 
  mutate(database = "PressReader (Connections)", publisher = "PressReader", metric_name = case_when(
    metric_name == "sessions" ~ "Sessions",
    metric_name == "article_opens" ~ "Views"
  )) %>% 
  filter(reporting_period > as.Date("2021-06-01"))
