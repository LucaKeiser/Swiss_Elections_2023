
# load packages and data --------------------------------------------------

# tidyverse
library(tidyverse)

# data
df <- readxl::read_xlsx(here::here("Data",
                                   "2023_10_22_Nationalratswahlen_2023_Budgetierte_Einnahmen_und_Zuwendungen.xlsx")) %>% 
  janitor::clean_names()




# data processing ---------------------------------------------------------

# take a look first
glimpse(df)

# only dates need to be fixed
df <- df %>% 
  mutate(datum = as_date(datum, origin = "1900-01-01") - 2,
         datum_des_exports = dmy(datum_des_exports))

# handle names
df <- df %>% 
  # create short party names
  mutate(partei_kurz = case_when(
    parteizugehorigkeit_mutterpartei == "Die Mitte" ~ "Mitte",
    parteizugehorigkeit_mutterpartei == "Eidgenössisch-Demokratische Union" ~ "EDU",
    parteizugehorigkeit_mutterpartei == "Evangelische Volkspartei der Schweiz" ~ "EVP",
    parteizugehorigkeit_mutterpartei == "FDP.Die Liberalen" ~ "FDP",
    parteizugehorigkeit_mutterpartei == "GRÜNE Schweiz" ~ "Grüne",
    parteizugehorigkeit_mutterpartei == "Grünliberale Partei" ~ "GLP",
    parteizugehorigkeit_mutterpartei == "Lega dei Ticinesi" ~ "Lega",
    parteizugehorigkeit_mutterpartei == "Schweizerische Volkspartei" ~ "SVP",
    parteizugehorigkeit_mutterpartei == "Sozialdemokratische Partei der Schweiz" ~ "SP",
    parteizugehorigkeit_mutterpartei == "Übrige politische Parteien" ~ "Übrige",
    TRUE ~ parteizugehorigkeit_mutterpartei)
  ) %>% 
  # create full name
  rename("nachname" = name) %>% 
  mutate(name = str_c(nachname, vorname, 
                      sep = " "),
         name = str_to_title(name))



# save --------------------------------------------------------------------

# rename/relocate
df <- df %>% 
  rename("partei" = parteizugehorigkeit_mutterpartei,
         "einnahmen_total" = gesamtbetrag_der_einnahmen_in_chf,
         "einnahmen_monetare_zuwendungen" = einnahmen_durch_monetare_zuwendungen_in_chf,
         "einnahmen_nicht_monetare_zuwendungen" = wert_der_einnahmen_durch_nichtmonetare_zuwendungen_in_chf,
         "einnahmen_veranstaltungen" = einnahmen_durch_veranstaltungen_in_chf,
         "einnahmen_gueter_dienstleistungen" = einnahmen_durch_den_verkauf_von_gutern_und_dienstleistungen_in_chf,
         "eigenmittel" = monetare_eigenmittel_in_chf) %>% 
  select(datum_des_exports, akteur, art_des_akteurs, 
         kampagne, kampagne_fur, name, kanton, partei, partei_kurz,
         einnahmen_total, eigenmittel, einnahmen_monetare_zuwendungen, 
         einnahmen_nicht_monetare_zuwendungen, einnahmen_veranstaltungen,
         einnahmen_gueter_dienstleistungen, offenlegungsmeldung) 

write_rds(x = df,
          file = here::here("Swiss_Elections_2023_SHINY",
                            "swiss_elections_2023.rds"))
