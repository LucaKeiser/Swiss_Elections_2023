

# load packages and data --------------------------------------------------

library(shiny)
library(shinythemes)
library(tidyverse)
library(scales)
library(glue)

theme_set(theme_minimal())

# data
elections_2023_single_politicians <- read_rds(here::here("swiss_elections_2023.rds")) %>% 
  filter(kampagne_fur == "Einzelperson") %>% 
  mutate(name_choices = paste(glue("{name} ({partei_kurz})")))

# get party color
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
  
  titlePanel(p(strong("Nationalratswahlen Schweiz 2023"), br(), "Wahlkampfbudget - Einzelpersonen")),
  
  br(),
  br(),
  
  ###### 1. SIDEBAR PANEL ###### 
  
  ### Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    
    sidebarPanel(
      
      selectInput(inputId = "party_name",
                  label = "Partei:",
                  choices = sort(unique(elections_2023_single_politicians$partei)),
                  multiple = TRUE,
                  selected = NULL),
      
      selectInput(inputId = "politician_name",
                  label = "Politiker*in:",
                  choices = sort(unique(elections_2023_single_politicians$name_choices)),
                  multiple = TRUE,
                  selected = NULL),
      
      actionButton(inputId = "action_button",
                   label = "ERGEBNISSE ANZEIGEN!"),
      
      br(),
      br(),
      
      p(strong("Hinweis 1:"), "Damit Politiker*innen im Dropdown-Menü angezeigt werden, wählen Sie bitte zuerst die entsprechende(n) Partei(en) aus.", br(),
        strong("Hinweis 2:"), "Sie können auch lediglich eine Partei anwählen. Dann werden sämtliche Kandidierende der entsprechenden Partei angezeigt.", br(),
        strong("Hinweis 3:"), "Wird nichts angewählt und auf 'ERGEBNISSE ANZEIGEN!' geklickt, werden alle 120 Personen im Datensatz dargestellt.", br(),
        strong("Hinweis 4:"), "Verwenden Sie bei der Benutzung mit dem Mobiltelefon bitte das Querformat.")
      
    ),
    
    
    ###### 2. MAIN PANEL ###### 
    
    tabsetPanel(
      
      type = "pills",
      
      # plots
      tabPanel(title = "Grafiken",
               br(),
               plotOutput("plot_1",
                          height = "1750px",
                          width = "75%"),
               hr(),
               plotOutput("plot_2",
                          height = "1750px",
                          width = "75%"),
               hr()),
      
      # data info
      tabPanel(title = "Daten",
               br(),
               dataTableOutput("data_table"))
      
    ),
    
  ),
  
  # caption
  br(),
  helpText("Shiny-App by ©Luca Keiser", br(), br(),
           "Die Daten wurden am 29.09.2023 von der Webseite der Eidgenössischen Finanzkontrolle", br(),
           "(https://politikfinanzierung.efk.admin.ch/app/de/campaign-financings) heruntergeladen.",
           align = "right")
)





# Server (aka back end) ---------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  ### 1. create reactive expression
  elections_2023_single_politicians_reactive <- eventReactive(input$action_button, {
    
    
    if(length(input$party_name) > 0 & 
       length(input$politician_name) > 0) {
      
      elections_2023_single_politicians %>% 
        filter(partei %in% input$party_name &
                 name_choices %in% input$politician_name)
      
    } else if(length(input$party_name) > 0) {
      
      elections_2023_single_politicians %>% 
        filter(partei %in% input$party_name)
      
    } else{
      
      elections_2023_single_politicians
      
    }
    
  })
  
  elections_2023_single_politicians_plot_2 <- reactive({
    
    elections_2023_single_politicians_reactive() %>% 
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
  
  
  ### 2. outputs
  
  # plot_1
  output$plot_1 <- renderPlot({
    
    elections_2023_single_politicians_reactive() %>%
      mutate(name = fct_reorder(name, einnahmen_total)) %>%
      ggplot(aes(einnahmen_total, name)) + 
      geom_col(aes(fill = partei_kurz)) + 
      geom_vline(xintercept = median(elections_2023_single_politicians$einnahmen_total),
                 lty = 2,
                 linewidth = 1.1,
                 alpha = 0.5) + 
      geom_vline(xintercept = mean(elections_2023_single_politicians$einnahmen_total),
                 lty = 1,
                 linewidth = 1.1,
                 alpha = 0.5) + 
      scale_x_continuous(labels = comma_format(big.mark = "`"),
                         breaks = seq(0, 400000, 50000)) + 
      scale_fill_manual(values = party_colors) +
      labs(title = "\nWelche Einzelperson hat am meisten Geld für die\njeweilige Wahlkampagne zur Verfügung?\n",
           fill = "Partei:",
           x = "\nMenge an zur Verfügung stehendem Geld in CHF\n",
           y = "",
           caption = glue("Gestrichelte Linie: Medianwert aller NR-Kandidierenden ({format(median(elections_2023_single_politicians$einnahmen_total), big.mark = '`')} CHF)\nDurchgezogene Linie: Durchschnittswert aller NR-Kandidierenden: ({format(round(mean(elections_2023_single_politicians$einnahmen_total)), big.mark = '`')} CHF)")) +
      theme(legend.position = "top",
            text = element_text(size = 15))
    
  })
  
  # plot_2
  output$plot_2 <- renderPlot({
    
    elections_2023_single_politicians_plot_2() %>% 
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
  
  
  
  # data_table
  output$data_table <- renderDataTable({
    elections_2023_single_politicians_reactive() %>% 
      select(-c(partei_kurz, name_choices))
  })
  
  
  
  ### 3. observer
  observe({
    
    new_politician_choices <- elections_2023_single_politicians %>% 
      filter(partei %in% input$party_name) %>% 
      pull(name_choices) %>% 
      unique() %>% 
      sort()
    
    
    updateSelectInput(session, 
                      inputId = "politician_name",
                      choices = new_politician_choices)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
