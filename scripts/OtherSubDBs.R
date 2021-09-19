OtherSubs <- read_excel("data/raw/Others/SubDBs_manual.xlsx", sheet="Form1") %>%
  select(
    database = "Please choose a subscription database",
    reporting_period = "Please select the start of the month you're providing data for (e.g. 1/3/2020 for March 2020)",
    searches = "How many searches were recorded?",
    views = "How many views were recorded?",
    turnaways = "How many turnaways were recorded?",
    sessions = "How many sessions were recorded?"
    ) %>%
  distinct() %>% 
  clean_and_gather(3:6, "metric_name") %>% 
  mutate(reporting_period = as.Date(reporting_period)) %>% 
  filter(database != "Discovery" & !is.na(value))

OtherSubs <- OtherSubs %>%
  group_by(database, metric_name) %>% 
  summarise(n = sum(value, na.rm = TRUE), .groups = "drop") %>%
  filter(n == 0) %>%
  distinct(across(1:2)) %>% 
  anti_join(x = OtherSubs, y = ., by = c("database", "metric_name")) %>% 
  mutate(publisher = case_when(
    database == "Activity Corner" ~ "Forward Learning",
    database == "Ancestry" ~ "ProQuest",
    database == "Beamafilm" ~ "Digitales",
    database == "British Newspaper Archive" ~ "Brightsolid",
    database == "BWB Texts Collection" ~ "Bridget Williams Books",
    database == "Carters" ~ "Digitales",
    database == "CultureGrams" ~ "ProQuest",
    database == "Discovery" ~ "Digitales",
    database == "Dragonsource" ~ "Dragon Source",
    database == "Early World of Learning" ~ "Forward Learning",
    database == "Enciclopedia Estudiantl Hallazgos" ~ "Forward Learning",
    database == "Findmypast.co.uk" ~ "Brightsolid",
    database == "Good Reading Online (includes SpineOut & PK Mag)" ~ "Goodreading",
    database == "Knowledge Basket - Maori Land Court Minute Books Index" ~ "Knowledge Basket",
    database == "Knowledge Basket - New Zealand Index" ~ "Knowledge Basket",
    database == "Knowledge Basket - Newztext" ~ "Knowledge Basket",
    database == "MyHeritage" ~ "EBSCO",
    database == "New Zealand Geographic TV" ~ "Kowhai Media",
    database == "NZX" ~ "NZX",
    database == "Road to IELTS - Academic" ~ "Bookery",
    database == "Road to IELTS - General" ~ "Bookery",
    database == "The Genealogist.co.uk" ~ "The Genealogist",
    database == "Treaty of Waitangi Collection" ~ "Bridget Williams Books",
    database == "TumbleBookCloud Junior" ~ "Digitales",
    database == "TumbleBooks" ~ "Digitales",
    database == "World Book Advanced" ~ "Forward Learning",
    database == "World Book Discover" ~ "Forward Learning",
    database == "World Book Kids" ~ "Forward Learning",
    database == "World Book Student" ~ "Forward Learning",
    database == "World Book Timelines" ~ "Forward Learning",
    database == "Lingogo" ~ "Little Mouse Co."
  ))
