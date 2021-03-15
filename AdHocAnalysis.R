DBs_to_check <- DatabaseSubs %>%
  filter(reporting_period >= "2018-03-01" & reporting_period < "2020-03-01" & !database %in% automated) %>% 
  group_by(database, publisher, metric_name) %>% 
  summarise(median_value = round(median(value), 0)) %>% 
  pivot_wider(names_from = metric_name, values_from = median_value, names_prefix = "Median_",) %>% 
  arrange(desc(Median_Sessions), desc(Median_Views)) %>% 
  filter_at(vars(starts_with("M")), any_vars(. > 100))