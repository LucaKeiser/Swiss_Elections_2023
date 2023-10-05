
# load packages and data --------------------------------------------------

# tidyverse
library(tidyverse)
library(glue)

# data
df <- readxl::read_xlsx(here::here("01_Data",
                                   "2023_10_22_Nationalratswahlen_2023_Budgetierte_Einnahmen_und_Zuwendungen.xlsx")) %>% 
  janitor::clean_names()





# data processing ---------------------------------------------------------

# take a look first
glimpse(df)


### 1. rename variables
df <- df %>% 
  rename("partei" = parteizugehorigkeit_mutterpartei,
         "einnahmen_total" = gesamtbetrag_der_einnahmen_in_chf,
         "einnahmen_monetare_zuwendungen" = einnahmen_durch_monetare_zuwendungen_in_chf,
         "einnahmen_nicht_monetare_zuwendungen" = wert_der_einnahmen_durch_nichtmonetare_zuwendungen_in_chf,
         "einnahmen_veranstaltungen" = einnahmen_durch_veranstaltungen_in_chf,
         "einnahmen_gueter_dienstleistungen" = einnahmen_durch_den_verkauf_von_gutern_und_dienstleistungen_in_chf,
         "eigenmittel" = monetare_eigenmittel_in_chf)


### 2. fix dates
df <- df %>% 
  mutate(datum = as_date(datum, origin = "1900-01-01") - 2,
         datum_des_exports = dmy(datum_des_exports))


### 3. handle names
df <- df %>% 
  # create short party names
  mutate(partei_kurz = case_when(
    partei == "Die Mitte" ~ "Mitte",
    partei == "Eidgenössisch-Demokratische Union" ~ "EDU",
    partei == "Evangelische Volkspartei der Schweiz" ~ "EVP",
    partei == "FDP.Die Liberalen" ~ "FDP",
    partei == "GRÜNE Schweiz" ~ "Grüne",
    partei == "Grünliberale Partei" ~ "GLP",
    partei == "Lega dei Ticinesi" ~ "Lega",
    partei == "Schweizerische Volkspartei" ~ "SVP",
    partei == "Sozialdemokratische Partei der Schweiz" ~ "SP",
    partei == "Übrige politische Parteien" ~ "Übrige",
    TRUE ~ partei)
  ) %>% 
  # create full name
  rename("nachname" = name) %>% 
  mutate(name = str_c(nachname, vorname, 
                      sep = " "),
         name = str_to_title(name)) %>% 
  # create names for drop downs (shiny-app)
  mutate(name_choices = paste(glue("{name} ({partei_kurz}, {kanton})"))) 


### 4. remove invalid observations (26 observations with no name)
df <- df %>% 
  filter(kampagne_fur != "Gruppe von Kandidierenden (Einzelauflistung nicht möglich)") %>% 
  # rename
  mutate(kampagne_fur = ifelse(kampagne_fur == "Gruppe von Kandidierenden (namentliche Auflistung)",
                               "Gruppe von Kandidierenden",
                               kampagne_fur))


### 5. count number of actors
df <- df %>% 
  add_count(name_choices, kampagne_fur,
            name = "n_appearances") %>% 
  mutate(anzahl_akteure = n_appearances) %>% 
  select(-n_appearances)


### 6. recalculate total
# NOTE: Some politicians appear in more than once in group-campaigns. 
#       To calculate the percentages the incomes are added together. 
df <- df %>% 
  group_by(name_choices, kampagne_fur) %>% 
  mutate(across(einnahmen_total:eigenmittel, ~sum(.))) %>% 
  ungroup()


### 7. recreate actors
actors_output <- tibble(
  name_choices  = vector(mode =  "character"),
  kampagne_fur = vector(mode = "character"),
  akteur_collapsed = vector(mode = "character")
)

politicians_of_interest <- df %>% 
  # only groups have multiple actors
  filter(kampagne_fur == "Gruppe von Kandidierenden") %>% 
  pull(name_choices)


for(i in seq_along(politicians_of_interest)) {
  
  if(i %% 100 == 0) {
    message(glue("{i}/{length(politicians_of_interest)}"))
  }
  
  temp <- tibble(
    name_choices = politicians_of_interest[i],
    kampagne_fur = "Gruppe von Kandidierenden",
    akteur_collapsed = df %>% 
      filter(name_choices == politicians_of_interest[i] & 
             kampagne_fur == "Gruppe von Kandidierenden") %>%
      pull(akteur) %>% 
      str_c(collapse = "; ")
  )
  
  actors_output <- bind_rows(actors_output, temp)
  
}

# clean up
rm(politicians_of_interest, i, temp)
actors_output <- actors_output %>% 
  distinct()


### 8. recreate campaign
campaign_output <- tibble(
  name_choices  = vector(mode =  "character"),
  kampagne_collapsed = vector(mode = "character")
)

politicians_of_interest <- df %>% 
  # only groups have multiple actors
  filter(kampagne_fur == "Gruppe von Kandidierenden") %>% 
  pull(name_choices)


for(i in seq_along(politicians_of_interest)) {
  
  if(i %% 100 == 0) {
    message(glue("{i}/{length(politicians_of_interest)}"))
  }
  
  temp <- tibble(
    name_choices = politicians_of_interest[i],
    kampagne_collapsed = df %>% 
      filter(name_choices == politicians_of_interest[i] & 
               kampagne_fur == "Gruppe von Kandidierenden") %>%
      pull(kampagne) %>% 
      str_c(collapse = ", ")
  ) %>% 
    # add "," for the last entry (this will be important later)
    mutate(kampagne_collapsed = str_c(kampagne_collapsed, ","))
  
  campaign_output <- bind_rows(campaign_output, temp)
  
}

# clean up
rm(politicians_of_interest, i, temp)
campaign_output <- campaign_output %>% 
  distinct()

## 8.1 delete duplicates (people can appear together in multiple campaigns...)
campaign_output_temp <- campaign_output %>% 
  separate_rows(kampagne_collapsed, 
                sep = "\\),") %>%
  mutate(kampagne_collapsed = str_squish(kampagne_collapsed)) %>% 
  distinct(name_choices, kampagne_collapsed) %>% 
  filter(kampagne_collapsed != "")

campaign_output <- tibble(
  name_choices = vector(mode = "character"),
  kampagne_fur = vector(mode = "character"),
  kampagne_collapsed = vector(mode = "character")
)

politicians_of_interest <- campaign_output_temp %>% 
  pull(name_choices) %>% 
  unique()

for(i in seq_along(politicians_of_interest)) {
  
  if(i %% 100 == 0) {
    message(glue("{i}/{length(politicians_of_interest)}"))
  }
  
  temp <- tibble(
    name_choices = politicians_of_interest[i],
    kampagne_fur = "Gruppe von Kandidierenden",
    kampagne_collapsed = campaign_output_temp %>% 
      filter(name_choices == politicians_of_interest[i]) %>% 
      pull(kampagne_collapsed) %>%  
      sort() %>% 
      str_c(collapse = "), ")
  ) %>% 
    mutate(kampagne_collapsed = str_c(kampagne_collapsed, ")"))
  
  campaign_output <- bind_rows(campaign_output, temp)
  
}

# clean up
rm(campaign_output_temp, politicians_of_interest, i, temp)
campaign_output <- campaign_output %>% 
  distinct()


### 9. merge

actors_output <- actors_output %>% 
  left_join(campaign_output, 
            by = c("name_choices", "kampagne_fur"))

df <- df %>% 
  left_join(actors_output, 
            by = c("name_choices", "kampagne_fur")) %>% 
  mutate(akteur_collapsed = ifelse(is.na(akteur_collapsed),
                                   akteur, 
                                   akteur_collapsed)) %>% 
  mutate(kampagne_collapsed = ifelse(is.na(kampagne_collapsed),
                                     kampagne,
                                     kampagne_collapsed)) %>% 
  distinct(name_choices, kampagne_fur, akteur_collapsed,
           .keep_all = TRUE)

### 9. add number of people within one campaign
df <- df %>% 
  mutate(anzahl_personen_kampagne = str_count(kampagne_collapsed, "\\),") + 1,
         kampagne_collapsed = str_replace_all(kampagne_collapsed, "\\),", "\\);"),
         kampagne_collapsed = str_replace_all(kampagne_collapsed, "Schweizerische Volkspartei", "SVP"), 
         kampagne_collapsed = str_replace_all(kampagne_collapsed, "FDP.Die Liberalen", "FDP"),
         kampagne_collapsed = str_replace_all(kampagne_collapsed, "Sozialdemokratische Partei der Schweiz", "SP"),
         kampagne_collapsed = str_replace_all(kampagne_collapsed, "Eidgenössisch-Demokratische Union ", "EDU"),
         kampagne_collapsed = str_replace_all(kampagne_collapsed, "Evangelische Volkspartei der Schweiz", "EVP"),
         kampagne_collapsed = str_replace_all(kampagne_collapsed, "GRÜNE Schweiz", "Grüne"),
         kampagne_collapsed = str_replace_all(kampagne_collapsed, "Grünliberale Partei", "GLP"),
         kampagne_collapsed = str_replace_all(kampagne_collapsed, "Lega dei Ticinesi", "Lega"),
         kampagne_collapsed = str_replace_all(kampagne_collapsed, "Übrige politische Parteien", "Übrige"))


### 10. temp name fix
# NOTE: some names appear twice (data entry issue? Some cases seem implausible...)
#       temporary solution for plots => add "(...) 2" to one of the names.

df <- df %>% 
  rowid_to_column()

# see which cases
df %>% 
  filter(kampagne_fur == "Gruppe von Kandidierenden") %>% 
  add_count(name) %>% 
  filter(n > 1) %>% 
  select(rowid, name, name_choices, einnahmen_total) %>% 
  arrange(name) %>% 
  View()

# "fix"
df <- df %>% 
  mutate(name = ifelse(rowid %in% c(83, 2085, 315, 561, 1465,
                                    1889, 646, 522, 1854),
                       glue("{name} 2"),
                       name)
  )

# check
df %>% 
  filter(kampagne_fur == "Gruppe von Kandidierenden") %>% 
  add_count(name) %>% 
  filter(n > 1)





# save --------------------------------------------------------------------

# select/relocate
df <- df %>% 
  select(datum_des_exports, akteur_collapsed, anzahl_akteure, 
         kampagne_fur, kampagne_collapsed, anzahl_personen_kampagne,
         name, name_choices, kanton, partei, partei_kurz,
         einnahmen_total, eigenmittel, einnahmen_monetare_zuwendungen, 
         einnahmen_nicht_monetare_zuwendungen, einnahmen_veranstaltungen,
         einnahmen_gueter_dienstleistungen)

# check
View(df)

# save
write_rds(x = df,
          file = here::here("03_Swiss_Elections_2023_SHINY",
                            "swiss_elections_2023.rds"))