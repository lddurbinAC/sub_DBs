LinkedInPrep <- function(x) {
  read_csv(x, col_types = "ccnnnnnnnnnnnnn") %>% 
    clean_and_gather(3:(ncol(.)), "metric_name") %>% 
    mutate(reporting_period = mdy(end_day_pst_pdt) %>% floor_date("month"), database = "LinkedIn Learning", publisher = "LinkedIn", metric_name = case_when(
      metric_name == "people_logged_in" ~ "sessions",
      metric_name == "courses_viewed" ~ "views",
      metric_name == "course_views" ~ "views",
    )) %>% 
    filter(!is.na(metric_name)) %>% 
    select(-c(1:2))
}

# Run the raw data files through the preparatory function and bind the data frames together
LinkedIn <- process_files("data/raw/LinkedInLearning", LinkedInPrep)
