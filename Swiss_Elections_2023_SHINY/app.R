

# load packages and data --------------------------------------------------

library(shiny)
library(shinythemes)
library(tidyverse)
library(scales)
library(glue)

theme_set(theme_minimal())

# data
elections_2023 <- read_rds(here::here("swiss_elections_2023.rds"))

elections_2023_single_person <- elections_2023 %>% 
  filter(kampagne_fur == "Einzelperson")

elections_2023_groups <- elections_2023 %>% 
  filter(kampagne_fur == "Gruppe von Kandidierenden")


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
  theme = shinytheme("sandstone"),
  
  
  ### Application title
  br(),
  br(),
  
  titlePanel(
    
    windowTitle = "Nationalratswahlen Schweiz 2023",
    title = p(strong("Nationalratswahlen Schweiz 2023"), br(), "Wahlkampfbudgets - Einzelpersonen & Gruppen von Kandidierenden")),
  
  
  br(),
  br(),
  
  
  ###### 1. SIDEBAR PANEL ###### 
  
  sidebarLayout(
    
    sidebarPanel(
      
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
      br(),
      
      p(strong("Hinweis 1:"), "Damit Politiker*innen im Dropdown-Menü angezeigt werden, wählen Sie bitte zuerst die entsprechende(n) Partei(en) aus.", br(),
        strong("Hinweis 2:"), "Sie können auch lediglich eine Partei anwählen. Dann werden sämtliche (!) Kandidierende der entsprechenden Partei angezeigt.", br(),
        strong("Hinweis 3:"), "Wird nichts angewählt und auf 'Ergebnisse Anzeigen!' geklickt, werden alle (!) Personen im jweiligen Datensatz dargestellt.", br(),
        strong("Hinweis 4:"), "Der Gesamtbetrag bei 'Gruppe von Kandidierenden' kommt der Gesamtgruppe und nicht (!) einer Person zu. Siehe 'Weitere Informationen zu den Kampagnen'", br(),
        strong("Hinweis 5:"), "Verwenden Sie bei der Benutzung mit dem Mobiltelefon bitte das Querformat.")
      
    ),
    
    
    ###### 2. MAIN PANEL ###### 
    
    tabsetPanel(
      
      type = "pills",
      
      # plots
      tabPanel(title = "Grafiken",
               br(),
               plotOutput("absolut_plot",
                          height = "1000px",
                          width = "75%"),
               hr(),
               plotOutput("percent_plot",
                          height = "1000px",
                          width = "75%"),
               hr()),
      
      # further information
      tabPanel(title = "Weitere Informationen zu den Kampagnen",
               br(),
               tableOutput("info"),
               hr()),
      
      # data info
      tabPanel(title = "Verwendete Daten",
               br(),
               dataTableOutput("data_table"))
      
    ),
    
  ),
  
  
  ###### 3. caption ###### 
  
  br(),
  helpText("Shiny-App by ©Luca Keiser", br(), br(),
           "Die Daten wurden am 29.09.2023 von der Webseite der Eidgenössischen Finanzkontrolle", br(),
           "(https://politikfinanzierung.efk.admin.ch/app/de/campaign-financings) heruntergeladen.",
           align = "right")
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
    
  })
  
  
  ###### 2. outputs ###### 
  
  # 1. info
  output$info <- renderTable({
    
    elections_2023_reactive() %>% 
      select(name_choices, anzahl_personen_kampagne, kampagne, akteur) %>% 
      mutate(kampagne = str_replace_all(kampagne, "\\),", "\\);"),
             anzahl_personen_kampagne = as.integer(anzahl_personen_kampagne)) %>% 
      rename("Name & Partei" = name_choices,
             "Wie viele Personen sind Teil der Kampagne?" = anzahl_personen_kampagne,
             "Welche Akteure finanzieren die Kampagne?" = akteur,
             "Welche Kandidiernde sind in der gleichen Kampagne?" = kampagne)
    
  })
  
  
  # 2. absolut_plot
  output$absolut_plot <- renderPlot({
    
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
      scale_x_continuous(labels = comma_format(big.mark = "`"),
                         breaks = seq(0, 1500000, 50000)) + 
      scale_fill_manual(values = party_colors) +
      labs(title = "\nWelche Einzelperson hat am meisten Geld für die\njeweilige Wahlkampagne zur Verfügung?\n",
           fill = "Partei:",
           x = "\nMenge an zur Verfügung stehendem Geld in CHF\n",
           y = "",
           caption = glue("Gestrichelte Linie: Medianwert aller (Einzelpersonen & Gruppen) NR-Kandidierenden ({format(median(elections_2023$einnahmen_total), big.mark = '`')} CHF)\nDurchgezogene Linie: Durchschnittswert aller (Einzelpersonen & Gruppen) NR-Kandidierenden ({format(round(mean(elections_2023$einnahmen_total)), big.mark = '`')} CHF)")) +
      theme(legend.position = "top",
            text = element_text(size = 15))
    
  })
  
  # 3. percent_plot
  output$percent_plot <- renderPlot({
    
    percent_plot() %>%
      mutate(name = fct_reorder(name, einnahmen_total)) %>%
      ggplot(aes(pct_values, name,
                 fill = pct_variables)) +
      geom_col(color = "white") +
      scale_x_continuous(labels = percent_format()) +
      scale_fill_manual(values = money_colors) +
      labs(title = "\nWie setzt sich das Geld für die\nWahlkampagne zusammen?\n",
           x = "",
           y = "",
           fill = "") +
      theme(legend.position = "top",
            text = element_text(size = 15)) +
      guides(fill = guide_legend(reverse = TRUE,
                                 ncol = 1))
    
  })
  
  # 4. data_table
  output$data_table <- renderDataTable({
    
    elections_2023_reactive()
    
  })
  
  
  ###### 3. observer ###### 
  observe({
    
    # 3.1.
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
    
    # 3.2.
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
