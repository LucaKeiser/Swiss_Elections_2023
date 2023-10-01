
# load packages and data --------------------------------------------------

# tidyverse
library(tidyverse)
library(glue)

# data
df <- readxl::read_xlsx(here::here("Data",
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
  mutate(name_choices = paste(glue("{name} ({partei_kurz})"))) 


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

for(i in seq_along(df$name_choices)) {
  
  if(i %% 100 == 0) {
    message(glue("{i}/{length(df$name_choices)}"))
  }
  
  temp <- tibble(
    name_choices = df$name_choices[i],
    kampagne_fur = "Gruppe von Kandidierenden",
    akteur_collapsed = df %>% 
      filter(# only groups have multiple actors
        kampagne_fur == "Gruppe von Kandidierenden",
        name_choices == df$name_choices[i]) %>% 
      pull(akteur) %>% 
      str_c(collapse = "; ")
  )
  
  actors_output <- bind_rows(actors_output, temp)
  
}

actors_output <- actors_output %>% 
  distinct()


### 8. merge
df <- df %>% 
  left_join(actors_output, 
            by = c("name_choices", "kampagne_fur")) %>% 
  mutate(akteur_collapsed = ifelse(is.na(akteur_collapsed),
                                   akteur, 
                                   akteur_collapsed)) %>% 
  distinct(name_choices, kampagne_fur, akteur_collapsed,
           .keep_all = TRUE) %>% 
  select(-akteur) %>% 
  rename("akteur" = akteur_collapsed)


### 9. add number of people within one campaign
df <- df %>% 
  mutate(anzahl_personen_kampagne = str_count(kampagne, "\\),") + 1)




# save --------------------------------------------------------------------

# select/relocate
df <- df %>% 
  select(datum_des_exports, akteur, anzahl_akteure, 
         kampagne_fur, kampagne, anzahl_personen_kampagne,
         name, name_choices, kanton, partei, partei_kurz,
         einnahmen_total, eigenmittel, einnahmen_monetare_zuwendungen, 
         einnahmen_nicht_monetare_zuwendungen, einnahmen_veranstaltungen,
         einnahmen_gueter_dienstleistungen)

# check
View(df)

# save
write_rds(x = df,
          file = here::here("Swiss_Elections_2023_SHINY",
                            "swiss_elections_2023.rds"))
