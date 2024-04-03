
# load packages and data --------------------------------------------------

library(tidyverse) 
library(tidygraph) 
library(ggraph) 
library(igraph) 
library(ggrepel)
library(plotly)
library(glue)

theme_set(theme_void())

`%ni%` <- negate(`%in%`)


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


## network analysis -------------------------------------------------------

### 1. create edgelist and network object
edgelist <- df %>% 
  select(akteur, partei_kurz) %>% 
  na.omit()

network <- graph.data.frame(edgelist,
                            directed = FALSE) %>% 
  as_tbl_graph()


### 2. add node attributes

# party attributes
node_attributes_party_1 <- df %>% 
  filter(kampagne_fur == "Einzelperson") %>% 
  select("name" = partei_kurz, 
         einnahmen_total) %>%
  group_by(name) %>% 
  mutate(einnahmen_total = sum(einnahmen_total)) %>% 
  distinct()

node_attributes_party_2 <- df %>% 
  filter(kampagne_fur == "Gruppe von Kandidierenden") %>% 
  select("name" = partei_kurz, 
         einnahmen_total) %>%
  distinct() %>% 
  group_by(name) %>% 
  mutate(einnahmen_total = sum(einnahmen_total)) %>% 
  distinct()

node_attributes_party <- node_attributes_party_1 %>% 
  left_join(node_attributes_party_2, 
            by = "name") %>% 
  mutate(einnahmen_total = einnahmen_total.x + einnahmen_total.y) %>% 
  select(-c(einnahmen_total.x, einnahmen_total.y))

# actor attributes
node_attributes_actor <- df %>% 
  select("name" = akteur, einnahmen_total) %>% 
  distinct() %>% 
  group_by(name) %>% 
  summarise(einnahmen_total = sum(einnahmen_total)) %>% 
  distinct()

# merge
node_attributes <- node_attributes_party %>% 
  bind_rows(node_attributes_actor)

### 3. define names to be displayed in the graph
display_names_party <- node_attributes_party$name
diaplay_names_actor <- c("Schweizerischer Gewerbeverband",
                         "GastroSuisse",
                         "Alliance Vaudoise",
                         "Campax",
                         "neo - Die sozialliberale Mitte",
                         "Mouvement Citoyens Genevois",
                         "HEV Kanton Zürich",
                         "Gewerkschaftsbund des Kantons Bern",
                         "Thurgauer Gewerbeverband",
                         "Hauseigentümer-Verband Kanton St. Gallen",
                         "Hauseigentümerverband Region Winterthur",
                         "auto-schweiz VEREINIGUNG SCHWEIZER AUTOMOBIL-IMPORTEURE")

### 4. create final network-object
network <- network %>% 
  activate(nodes) %>%
  left_join(node_attributes,
            by = "name") %>%
  mutate(color = ifelse(name %in% node_attributes_party$name,
                        name,
                        NA_character_),
         display_name_party = ifelse(name %in% display_names_party,
                                     name,
                                     NA_character_),
         display_name_actor = ifelse(name %in% diaplay_names_actor,
                                     name, 
                                     NA_character_))

# check
network %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  View()


## graph -------------------------------------------------------------------
set.seed(123)

network_plot <- network %>% 
  ggraph(layout = "fr") +
  geom_edge_link(color = "grey") + 
  geom_node_point(aes(color = color,
                      size = einnahmen_total)) +
  geom_node_label(aes(label = display_name_actor),
                  repel = TRUE,
                  fill = "#D5C2A5",
                  size = 3.5) +
  geom_node_label(aes(label = display_name_party),
                  repel = TRUE,
                  size = 4.5) +
  scale_size(range = c(1, 40),
             labels = scales::dollar_format(big.mark = "'",
                                            prefix = NULL,
                                            suffix = " CHF")) +
  scale_color_manual(values = c(
    "SVP" = "#00A04F",
    "SP" = "#E40326",
    "FDP" = "#104FA0",
    "Mitte" = "#F1920E",
    "Grüne" = "#84B419",
    "GLP" = "#49A3DE",
    "EVP" = "#FFDD00",
    "Lega" = "#1664B7",
    "EDU" = "black",
    "Unabhängig" = "grey30",
    "Übrige" = "grey50"
  )) +
  labs(title = "\nGesamtübersicht finanzielle Kampagnenmittel",
       subtitle = "Sämtliche Parteien und andere Akteuere im Datensatz\n",
       color = "Partei:",
       size = "Kampagneneinnahmen (Parteien)\nbzw. -ausgaben (Akteure):") +
  guides(color = "none",
         shape = "none") + 
  theme(legend.position = "bottom",
        text = element_text(size = 13.5))

# take a look
network_plot  





# save --------------------------------------------------------------------
write_rds(x = network_plot,
          file = here::here("03_Swiss_Elections_2023_SHINY",
                            "swiss_elections_2023_network_plot.rds"))
