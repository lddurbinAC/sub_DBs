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
  mutate(database = "PressReader", publisher = "PressReader")


# comparison_data <- PressReader_new %>% 
#   mutate(database = "PressReader_new") %>% 
#   bind_rows(PressReader) %>% 
#   mutate(metric_name = case_when(
#     metric_name == "article_opens" ~ "views",
#     !is.na(metric_name) ~ str_to_lower(metric_name)
#   )) %>% 
#   filter(reporting_period > date("2019-06-30")) 
# 
# comparison_data %>% 
#   ggplot(aes(x = reporting_period, y = value, fill = database)) +
#   geom_col(position = "dodge") +
#   facet_grid(rows = vars(metric_name), scales = "free_y")
# 
# comparison_data %>%
#   pivot_wider(names_from = database, values_from = value) %>% 
#   mutate(change = round(((PressReader_new - PressReader)/PressReader)*100), 2) %>% 
#   ggplot(aes(x = reporting_period, y = change)) +
#   geom_col() +
#   facet_grid(rows = vars(metric_name), scales = "free_y") +
#   geom_text(aes(label = change), vjust = -0.5)