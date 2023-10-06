

# load packages and data --------------------------------------------------

library(shiny)
library(shinythemes)
library(tidyverse)
library(tidygraph) 
library(ggraph) 
library(igraph) 
library(ggrepel)
library(scales)
library(skimr)
library(janitor)
library(glue)
library(shinycssloaders)

theme_set(theme_minimal())

# data
elections_2023 <- read_rds(here::here("swiss_elections_2023.rds"))

elections_2023_single_person <- elections_2023 %>% 
  filter(kampagne_fur == "Einzelperson")

elections_2023_groups <- elections_2023 %>% 
  filter(kampagne_fur == "Gruppe von Kandidierenden")

# network-object
network_plot <- read_rds(here::here("swiss_elections_2023_network_plot.rds"))


# define party colors
party_colors <- c(SVP = "#00A04F",
                  SP = "#E40326",
                  FDP = "#104FA0",
                  Mitte = "#F1920E",
                  Grüne = "#84B419",
                  GLP = "#49A3DE",
                  EVP = "#FFDD00",
                  Lega = "#1664B7",
                  EDU = "black",
                  Unabhängig = "grey30",
                  Übrige = "grey50")

# define money colors
money_colors <- c(`Eigenmittel` = "#86CA78",
                  `Monetäre Zuwendungen` = "#B4D4DA",
                  `Wert nicht monetärer Zuwendungen` = "#3F8BBA",
                  `Einnahmen Veranstaltungen` = "#1C5F9E",
                  `Einnahmen Güter und Dienstleistungen` = "#26456E")





# User interface (aka front end) ------------------------------------------

ui <- fluidPage(
  
  
  ### select a theme
  theme = shinytheme("paper"),
  
  
  ### Application title
  br(),
  br(),
  
  titlePanel(
    
    windowTitle = "Nationalratswahlen Schweiz 2023",
    title = p(strong("Nationalratswahlen in der Schweiz (2023)"), br(), "Kampagnenbudgets - Einzelpersonen & Gruppen von Kandidierenden")),
  
  br(),
  br(),
  
  
  ###### 1. SIDEBAR PANEL ###### 
  
  sidebarLayout(
    
    sidebarPanel(
      
      p(strong("Bitte auswählen und 'Ergebnisse Anzeigen!' klicken")),
      
      selectInput(inputId = "campaign_for",
                  label = "Kampagne für:",
                  choices = sort(unique(elections_2023$kampagne_fur)),
                  multiple = FALSE,
                  selected = "Einzelperson"),
      
      selectInput(inputId = "party_name",
                  label = "Partei:",
                  choices = sort(unique(elections_2023$partei)),
                  multiple = TRUE,
                  selected = NULL),
      
      selectInput(inputId = "politician_name",
                  label = "Politiker*in:",
                  choices = character(0),
                  multiple = TRUE,
                  selected = NULL),
      
      actionButton(inputId = "action_button",
                   label = "Ergebnisse Anzeigen!"),
      
      br(),
      hr(),
      
      p(strong("Hinweis 1:"), "Damit Politiker*innen im Dropdown-Menü angezeigt werden, wählen Sie bitte zuerst die entsprechende(n) Partei(en) aus.", br(),
        strong("Hinweis 2:"), "Sie können auch lediglich eine Partei anwählen. Dann werden sämtliche Kandidierende (max. 100) der entsprechenden Partei angezeigt.", br(),
        strong("Hinweis 3:"), "Wird nichts angewählt und auf 'Ergebnisse Anzeigen!' geklickt, werden sämltiche Kandidierende (max. 100) im jweiligen Datensatz dargestellt.", br(),
        strong("Hinweis 4:"), "Der Gesamtbetrag bei 'Gruppe von Kandidierenden' kommt der Gesamtgruppe und nicht einer einzelnen Person zu (siehe 'Weitere Informationen zu den Kampagnen').", br(),
        strong("Hinweis 5:"), "Verwenden Sie bei der Benutzung mit dem Mobiltelefon bitte das Querformat (Verwendung am Computer empfohlen)."),
      
      hr()
      
    ),
    
    
    ###### 2. MAIN PANEL ###### 
    mainPanel(
      
      tabsetPanel(
        
        type = "pills",
        
        # bar charts
        tabPanel(title = "Balkendiagramme",
                 br(),
                 fluidRow(column(plotOutput("absolut_plot",
                                            height = "1250px") %>% 
                                   withSpinner(), 
                                 width = 12)),
                 hr(),
                 fluidRow(column(plotOutput("percent_plot",
                                            height = "1250px") %>% 
                                   withSpinner(),
                                 width = 12)),
                 hr()),
        
        # further information
        tabPanel(title = "Weitere Informationen zu den Kampagnen",
                 br(),
                 tableOutput("info") %>% 
                   withSpinner(),
                 hr()),
        
        # network plot
        tabPanel(title = "Netzwerkgrafik",
                 br(),
                 fluidRow(column(plotOutput("network_plot",
                                            height = "1000px") %>% 
                                   withSpinner(),
                                 width = 12))),
        
        # summary
        tabPanel(title = "Datenübersicht/Summary",
                 br(),
                 br(),
                 p(strong(glue("...des aktuellen Teildatensatzes:"))),
                 br(),
                 textOutput("summary_current_text"),
                 br(),
                 tableOutput("summary_current_numeric")%>% 
                   withSpinner(),
                 br(),
                 br(),
                 hr(),
                 br(),
                 br(),
                 br(),
                 p(strong("...des gesamten Datensatzes:")),
                 br(),
                 textOutput("summary_overall_text"),
                 br(),
                 tableOutput("summary_overall_numeric"),
                 hr())
        
      ),
      
      ###### 3. caption ###### 
      
      br(),
      helpText("Die Daten wurden am 02.10.2023 aktualisiert. Siehe Webseite der Eidgenössischen Finanzkontrolle:", br(),
               "https://politikfinanzierung.efk.admin.ch/app/de/campaign-financings."),
      helpText("Shiny-App by ©Luca Keiser | Oktober, 2023", br(),
               "Code: https://github.com/LucaKeiser/Swiss_Elections_2023")
      
    )
    
  )
  
)





# Server (aka back end) ---------------------------------------------------

server <- function(input, output, session) {
  
  
  ###### 1. create reactive expressions ###### 
  
  # 1.1.
  elections_2023_reactive <- eventReactive(input$action_button, {
    
    # single person
    if(input$campaign_for == "Einzelperson") {
      
      
      if(length(input$party_name) > 0 & 
         length(input$politician_name) > 0) {
        
        elections_2023_single_person %>% 
          filter(partei %in% input$party_name &
                   name_choices %in% input$politician_name)
        
      } else if(length(input$party_name) > 0) {
        
        elections_2023_single_person %>% 
          filter(partei %in% input$party_name)
        
      } else{
        
        elections_2023_single_person
        
      }
      
    }
    
    # groups
    else {
      
      if(length(input$party_name) > 0 & 
         length(input$politician_name) > 0) {
        
        elections_2023_groups %>% 
          filter(partei %in% input$party_name &
                   name_choices %in% input$politician_name)
        
      } else if(length(input$party_name) > 0) {
        
        elections_2023_groups %>% 
          filter(partei %in% input$party_name)
        
      } else{
        
        elections_2023_groups
        
      }
      
    }
    
  })
  
  
  # 1.2.
  percent_plot <- reactive({
    
    if(nrow(elections_2023_reactive()) > 100) {
      
      elections_2023_reactive() %>% 
        top_n(einnahmen_total, n = 100) %>% 
        mutate(`Monetäre Zuwendungen` = einnahmen_monetare_zuwendungen /
                 einnahmen_total,
               `Wert nicht monetärer Zuwendungen` = einnahmen_nicht_monetare_zuwendungen /
                 einnahmen_total,
               `Einnahmen Veranstaltungen` = einnahmen_veranstaltungen /
                 einnahmen_total,
               `Einnahmen Güter und Dienstleistungen` = einnahmen_gueter_dienstleistungen /
                 einnahmen_total,
               `Eigenmittel` = eigenmittel /
                 einnahmen_total) %>% 
        pivot_longer(cols = `Monetäre Zuwendungen`:`Eigenmittel`,
                     names_to = "pct_variables",
                     values_to = "pct_values") %>% 
        mutate(pct_variables = fct_relevel(pct_variables, 
                                           "Einnahmen Güter und Dienstleistungen",
                                           "Einnahmen Veranstaltungen",
                                           "Wert nicht monetärer Zuwendungen",
                                           "Monetäre Zuwendungen",
                                           "Eigenmittel")) 
      
    } else {
      
      elections_2023_reactive() %>% 
        mutate(`Monetäre Zuwendungen` = einnahmen_monetare_zuwendungen /
                 einnahmen_total,
               `Wert nicht monetärer Zuwendungen` = einnahmen_nicht_monetare_zuwendungen /
                 einnahmen_total,
               `Einnahmen Veranstaltungen` = einnahmen_veranstaltungen /
                 einnahmen_total,
               `Einnahmen Güter und Dienstleistungen` = einnahmen_gueter_dienstleistungen /
                 einnahmen_total,
               `Eigenmittel` = eigenmittel /
                 einnahmen_total) %>% 
        pivot_longer(cols = `Monetäre Zuwendungen`:`Eigenmittel`,
                     names_to = "pct_variables",
                     values_to = "pct_values") %>% 
        mutate(pct_variables = fct_relevel(pct_variables, 
                                           "Einnahmen Güter und Dienstleistungen",
                                           "Einnahmen Veranstaltungen",
                                           "Wert nicht monetärer Zuwendungen",
                                           "Monetäre Zuwendungen",
                                           "Eigenmittel")) 
      
    }
    
  })
  
  
  ###### 2. outputs ###### 
  
  # 2.1. absolut_plot
  output$absolut_plot <- renderPlot({
    
    if(nrow(elections_2023_reactive()) > 100) {
      
      elections_2023_reactive() %>%
        top_n(einnahmen_total, n = 100) %>% 
        mutate(name = fct_reorder(name, einnahmen_total)) %>%
        ggplot(aes(einnahmen_total, name)) + 
        geom_col(aes(fill = partei_kurz)) + 
        geom_vline(xintercept = median(elections_2023$einnahmen_total),
                   lty = 2,
                   linewidth = 1.1,
                   alpha = 0.5) + 
        geom_vline(xintercept = mean(elections_2023$einnahmen_total),
                   lty = 1,
                   linewidth = 1.1,
                   alpha = 0.5) + 
        expand_limits(x = 0) +
        scale_x_continuous(labels = comma_format(big.mark = "'")) + 
        scale_fill_manual(values = party_colors) +
        labs(title = "\nWie viel Geld steht für die jeweilige Wahlkampagne zur Verfügung?",
             subtitle = "Aus Darstellungsgründen werden nachfolgend nur die ersten 100 Kandidierenden angezeigt.\n",
             fill = "Partei:",
             x = "\nMenge an zur Verfügung stehendem Geld in CHF\n",
             y = "",
             caption = glue("Gestrichelte Linie: Medianwert aller (Einzelpersonen & Gruppen) NR-Kandidierenden ({format(median(elections_2023$einnahmen_total), big.mark = '\\'')} CHF)\nDurchgezogene Linie: Durchschnittswert aller (Einzelpersonen & Gruppen) NR-Kandidierenden ({format(round(mean(elections_2023$einnahmen_total)), big.mark = '\\'')} CHF)")) +
        theme(legend.position = "top",
              text = element_text(size = 13.5))
      
    } else {
      
      elections_2023_reactive() %>%
        mutate(name = fct_reorder(name, einnahmen_total)) %>%
        ggplot(aes(einnahmen_total, name)) + 
        geom_col(aes(fill = partei_kurz)) + 
        geom_vline(xintercept = median(elections_2023$einnahmen_total),
                   lty = 2,
                   linewidth = 1.1,
                   alpha = 0.5) + 
        geom_vline(xintercept = mean(elections_2023$einnahmen_total),
                   lty = 1,
                   linewidth = 1.1,
                   alpha = 0.5) + 
        expand_limits(x = 0) +
        scale_x_continuous(labels = comma_format(big.mark = "'")) +
        scale_fill_manual(values = party_colors) +
        labs(title = "\nWie viel Geld steht für die jeweilige Wahlkampagne zur Verfügung?\n",
             fill = "Partei:",
             x = "\nMenge an zur Verfügung stehendem Geld in CHF\n",
             y = "",
             caption = glue("Gestrichelte Linie: Medianwert aller (Einzelpersonen & Gruppen) NR-Kandidierenden ({format(median(elections_2023$einnahmen_total), big.mark = '\\'')} CHF)\nDurchgezogene Linie: Durchschnittswert aller (Einzelpersonen & Gruppen) NR-Kandidierenden ({format(round(mean(elections_2023$einnahmen_total)), big.mark = '\\'')} CHF)")) +
        theme(legend.position = "top",
              text = element_text(size = 13.5))
      
    }
    
  })
  
  # 2.2. percent_plot
  output$percent_plot <- renderPlot({
    
    
    if((input$campaign_for == "Einzelperson" & nrow(percent_plot()) > 500) | 
      (input$campaign_for == "Gruppe von Kandidierenden" & nrow(percent_plot()) > 500)) {
      
      percent_plot() %>%
        mutate(name = fct_reorder(name, einnahmen_total)) %>%
        ggplot(aes(pct_values, name,
                   fill = pct_variables)) +
        geom_col(color = "white") +
        scale_x_continuous(labels = percent_format()) +
        scale_fill_manual(values = money_colors) +
        labs(title = "\nWie setzt sich das Geld für die jeweilige Wahlkampagne zusammen?",
             subtitle = "Aus Darstellungsgründen werden nachfolgend nur die ersten 100 Kandidierenden angezeigt.\n",
             x = "",
             y = "",
             fill = "") +
        theme(legend.position = "top",
              text = element_text(size = 13.5)) +
        guides(fill = guide_legend(reverse = TRUE,
                                   ncol = 1))
      
    } else {
      
      percent_plot() %>%
        mutate(name = fct_reorder(name, einnahmen_total)) %>%
        ggplot(aes(pct_values, name,
                   fill = pct_variables)) +
        geom_col(color = "white") +
        scale_x_continuous(labels = percent_format()) +
        scale_fill_manual(values = money_colors) +
        labs(title = "\nWie setzt sich das Geld für die jeweilige Wahlkampagne zusammen?\n",
             x = "",
             y = "",
             fill = "") +
        theme(legend.position = "top",
              text = element_text(size = 13.5)) +
        guides(fill = guide_legend(reverse = TRUE,
                                   ncol = 1))      
      
    }
    
  })
  
  
  # 2.3. info
  output$info <- renderTable({
    
    elections_2023_reactive() %>% 
      select(name_choices, einnahmen_total, akteur_collapsed, 
             anzahl_personen_kampagne, kampagne_collapsed) %>% 
      mutate(einnahmen_total = format(einnahmen_total, 
                                      big.mark = "'"),
             anzahl_personen_kampagne = as.integer(anzahl_personen_kampagne)) %>% 
      rename("Name & Partei der betrachteten Person" = name_choices,
             "Gesamtes Budget für die Kampagne(n) (in CHF)" = einnahmen_total,
             "Welche Akteure finanzieren die Kampagne(n)?" = akteur_collapsed,
             "Wie viele Kandidierende sind Teil derselben Kampagne(n)?" = anzahl_personen_kampagne,
             "Welche Kandidierende sind Teil der gleichen Kampagne(n)?" = kampagne_collapsed) %>% 
      arrange(`Name & Partei der betrachteten Person`)
    
  })
  
  
  # 2.4. network_plot
  output$network_plot <- renderPlot({
    
    suppressWarnings(
      network_plot
    )
    
  })
  
  
  # 2.5. data summary
  
  # 2.5.1
  output$summary_current_text <- renderText({
    
    glue("Es befinden sich {nrow(elections_2023_reactive())} Kandidierende im aktuellen Teildatensatz.")
    
  })
  
  output$summary_current_numeric <- renderTable({
    elections_2023_reactive() %>%
      skim() %>% 
      as_tibble() %>% 
      filter(skim_type == "numeric") %>% 
      select(-c(skim_type, n_missing, complete_rate, numeric.sd)) %>% 
      remove_empty(which = "cols") %>%
      mutate(across(numeric.mean:numeric.p100, ~round(.)),
             across(numeric.mean:numeric.p100, ~format(., big.mark = "'")),
             skim_variable = case_when(
               skim_variable == "anzahl_akteure" ~ "Wie viele Akteure unterstützen die Kampagne(n)?",
               skim_variable == "anzahl_personen_kampagne" ~ "Wie viele Kandidierende sind Teil derselben Kampagne(n)?",
               skim_variable == "einnahmen_total" ~ "Gesamtes Budget für die Kampagne(n) (in CHF)",
               skim_variable == "eigenmittel" ~ "Eigenmittel (in CHF)",
               skim_variable == "einnahmen_monetare_zuwendungen" ~ "Monetäre Zuwendungen (in CHF)",
               skim_variable == "einnahmen_nicht_monetare_zuwendungen" ~ "Wert nicht monetärer Zuwendungen (in CHF)",
               skim_variable == "einnahmen_veranstaltungen" ~ "Einnahmen Veranstaltungen (in CHF)",
               skim_variable == "einnahmen_gueter_dienstleistungen" ~ "Einnahmen Güter und Dienstleistungen (in CHF)"
             )) %>% 
      rename(" " = skim_variable,
             "Durchschnitt" = numeric.mean,
             "Minimum" = numeric.p0,
             "unteres Quartil" = numeric.p25,
             "mittleres Quartil (Median)" = numeric.p50,
             "oberes Quartil" = numeric.p75,
             "Maximum" = numeric.p100,
             "Histogramm" = numeric.hist)
  })
  
  # 2.5.2.
  output$summary_overall_text <- renderText({
    
    glue("Insgesamt befinden sich {nrow(elections_2023)} Kandidierende im gesamten Datensatz.")
    
  })
  
  
  output$summary_overall_numeric <- renderTable({
    elections_2023 %>% 
      skim() %>% 
      as_tibble() %>% 
      filter(skim_type == "numeric") %>% 
      select(-c(skim_type, n_missing, complete_rate, numeric.sd)) %>% 
      remove_empty(which = "cols") %>%
      mutate(across(numeric.mean:numeric.p100, ~round(.)),
             across(numeric.mean:numeric.p100, ~format(., big.mark = "'")),
             skim_variable = case_when(
               skim_variable == "anzahl_akteure" ~ "Wie viele Akteure unterstützen die Kampagne(n)?",
               skim_variable == "anzahl_personen_kampagne" ~ "Wie viele Kandidierende sind Teil derselben Kampagne(n)?",
               skim_variable == "einnahmen_total" ~ "Gesamtes Budget für die Kampagne(n) (in CHF)",
               skim_variable == "eigenmittel" ~ "Eigenmittel (in CHF)",
               skim_variable == "einnahmen_monetare_zuwendungen" ~ "Monetäre Zuwendungen (in CHF)",
               skim_variable == "einnahmen_nicht_monetare_zuwendungen" ~ "Wert nicht monetärer Zuwendungen (in CHF)",
               skim_variable == "einnahmen_veranstaltungen" ~ "Einnahmen Veranstaltungen (in CHF)",
               skim_variable == "einnahmen_gueter_dienstleistungen" ~ "Einnahmen Güter und Dienstleistungen (in CHF)"
             )) %>% 
      rename(" " = skim_variable,
             "Durchschnitt" = numeric.mean,
             "Minimum" = numeric.p0,
             "unteres Quartil" = numeric.p25,
             "mittleres Quartil (Median)" = numeric.p50,
             "oberes Quartil" = numeric.p75,
             "Maximum" = numeric.p100,
             "Histogramm" = numeric.hist)
  })
  
  
  ###### 3. observers ###### 
  
  # 3.1.
  observe({
    
    if(input$campaign_for == "Einzelperson") {
      
      new_party_choices <- elections_2023_single_person %>%
        pull(partei) %>%
        unique() %>%
        sort()
      
      updateSelectInput(session,
                        inputId = "party_name",
                        choices = new_party_choices)
    }
    
    else {
      
      new_party_choices <- elections_2023_groups %>%
        pull(partei) %>%
        unique() %>%
        sort()
      
      updateSelectInput(session,
                        inputId = "party_name",
                        choices = new_party_choices)
      
    }
    
  })
  
  # 3.2.
  observe({
    
    if(input$campaign_for == "Einzelperson") {
      
      new_politician_choices <- elections_2023_single_person %>%
        filter(partei %in% input$party_name) %>%
        pull(name_choices) %>%
        unique() %>%
        sort()
      
      updateSelectizeInput(session,
                           inputId = "politician_name",
                           choices = new_politician_choices,
                           server = TRUE)
    }
    
    else {
      
      new_politician_choices <- elections_2023_groups %>%
        filter(partei %in% input$party_name) %>%
        pull(name_choices) %>%
        unique() %>%
        sort()
      
      updateSelectizeInput(session,
                           inputId = "politician_name",
                           choices = new_politician_choices,
                           server = TRUE)
      
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)