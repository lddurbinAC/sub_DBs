#data for these subscription databases NOT entered in Microsoft Forms
automated <- c(
  "ABI/INFORM Global",
  "Academic Search Complete",
  "Australia & New Zealand Newsstream",
  "Australia / New Zealand Ref Centre",
  "Ethnographic Video Online: Vol III",
  "GreenFILE",
  "Oxford (Grove) Art Online",
  "Oxford (Grove) Music Online",
  "Hospitality and tourism complete",
  "JSTOR (Aluka Africa Collection and Public Library III)",
  "LGBT Thought and Culture",
  "LISTA",
  "Lynda.com",
  "MasterFILE Premier",
  "Music and dance online",
  "Music Online: Listening",
  "Naxos Music Library",
  "Naxos Music Library: Jazz",
  "Oxford English Dictionary",
  "PressReader",
  "The Guardian and The Observer (1791-2003)",
  "The Irish Times (1859-2010)",
  "The New York Times (1851-2007)",
  "The Scotsman (1817-1950)",
  "The Times of India",
  "Proquest Research Library",
  "SciTech Premium Collection(formerly Science Database)",
  "Standards Online New Zealand",
  "The Vogue Archive"
)

#get the sub DBs data
df <- as.data.frame(read_excel("data/raw/MASTER - DATABASE STATS & LOGINS (1).xlsm", sheet="Data", skip=7)) %>%
  remove_empty(which = c("rows", "cols")) %>% 
  select(1, 2, contains("sessions"), contains("searches"), contains("views"), contains("turnaways"))

HistoricSubDBs <- clean_and_gather(df, 3:ncol(df), "indicator") %>% 
  filter(value != "NA")
HistoricSubDBs $Month <- month.abb[as.numeric(substr(HistoricSubDBs $indicator, 6, 7))]
HistoricSubDBs $Year <- substr(HistoricSubDBs $indicator, 2, 5)
HistoricSubDBs $reporting_period <- as.Date(paste(HistoricSubDBs $Year, HistoricSubDBs $Month, "01", sep="-"), format="%Y-%b-%d")
HistoricSubDBs $metric_name <-str_extract(substr(HistoricSubDBs $indicator, 9, nchar(HistoricSubDBs $indicator)), "[A-Za-z]+" )
HistoricSubDBs  <-  dplyr::rename(HistoricSubDBs , "publisher" = "vendor") %>%
  select(database, publisher, metric_name, value, reporting_period) %>%
  filter(!database %in% automated) %>% 
  mutate(database = case_when(
    str_detect(database, "Communications and Mass") ~ "Gale OneFile: Communications and Mass Media",
    !is.na(database) ~ database),
    publisher = case_when(
      publisher == "Oxford" ~ "Oxford University Press",
      publisher == "Ancestry" ~ "ProQuest",
      publisher == "Genealogy supplies" ~ "The Genealogist",
      publisher == "DigitalES" ~ "Digitales",
      str_detect(publisher, "Gale") ~ "Gale Cengage",
      !is.na(publisher) ~ publisher,
    )
  )
