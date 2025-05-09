## app.R ##
library(shinydashboard)
library(readr)
library(tidyr)
library(dplyr)
library(plotly)
library(viridis)
library(lightgbm)
library(RCurl)
library(stringr)
library(lubridate)

# paleta de cores
names_map <- c('ADENO' = 'Adenovirus', 'BOCA' = 'Bocavirus', 'FLUA' = 'Influenza A',
               'FLUB' = 'Influenza B', 'METAP' = 'Metapneumovirus', 'OUTROS' = 'Other Viruses',
               'PARA1' = 'Parainfluenza 1', 'PARA2' = 'Parainfluenza 2',
               'PARA3' = 'Parainfluenza 3', 'PARA4' = 'Parainfluenza 4',
               'RINO' = 'Rhinovirus', 'SARS2' = 'SARS-CoV-2', 'VSR' = 'RSV') %>% rev
virus <- names(names_map)
cores <- c('#00CC96', '#19D3F3',  '#FFA15A', '#FECB52', '#B6E880', '#FF97FF', '#FF6692', '#FF88A6','#FFA8BD', '#FFC9D1', '#AB63FA', '#636EFA', '#EF553B') %>% rev
palette <- setNames(cores, names_map)

## Prevalencia ----
df_municipios <- read_csv("https://raw.githubusercontent.com/joao-1988/PySRAG_auto/main/df_municipios.csv") %>%
  # Forçar que a capital seja a primeira a ser exibida e as demais cidades seguirem o tamanho populacional
  mutate(ordenamento = case_when(MUNICIPIO == CAPITAL ~ exp(POPULACAO), .default = POPULACAO) ) %>%
  arrange(ESTADO,-ordenamento)
df_semanas <- read_csv("https://raw.githubusercontent.com/joao-1988/PySRAG_auto/main/df_semanas.csv")
# Vetores para seleção da região
estado <- df_municipios$ESTADO %>% unique %>% sort

## Prevalencia Pontual ----
url_booster <- "https://raw.githubusercontent.com/joao-1988/PySRAG_auto/main/booster.txt"
download.file(url_booster, destfile = "booster.txt", method = 'curl')
booster = lgb.load('booster.txt') # So resolver a incompatibilidade gerada ao fazer o download com download.file
classes <- read_csv("https://raw.githubusercontent.com/joao-1988/PySRAG_auto/main/classes.csv")
feature_name <- read_csv("https://raw.githubusercontent.com/joao-1988/PySRAG_auto/main/feature_name.csv")

## Data de atualizacao ----
# Extraindo as datas
filename <- read_csv("https://raw.githubusercontent.com/joao-1988/PySRAG_auto/main/filename.csv")
filename <- filename %>%
  mutate(date = str_extract(filename, "\\d{2}-\\d{2}-\\d{4}"),
         date = dmy(date)) # Convertendo para formato Date
# Encontrando a data mais recente
latest_update <- filename %>%
  summarise(latest_date = max(date, na.rm = TRUE)) %>%
  pull(latest_date)
# Convertendo a data mais recente para string
latest_update_str <- format(latest_update, "%Y/%m/%d")

## Header ----
header <- dashboardHeader(title = "SARI monitoring", titleWidth = 600)

## Sidebar Menu ----
sidebar <- dashboardSidebar(collapsed = FALSE, 
                            sidebarMenu(
                              menuItem("Home Page", tabName = "inicial", icon = icon("home")),
                              menuItem("Point Distribution", tabName = "prevalencia_pontual", icon = icon("chart-bar"),selected = TRUE)
                            )
)

## Prevalencia Pontual ----

radioButtons_view <- radioButtons("radio_view", label = h3("Visualization"), choices = list("Last 12 months" = "last_12", "All months available" = "all"),
                                    selected = "last_12")

selectInput_estado_pontual <- selectInput("select_estado_pontual", h3("State"), selected = 'São Paulo', 
                                          choices = estado)

sliderInput_idade_pontual <- sliderInput("input_idade_pontual", h3("Age (years)"), min=0,max=120, value = 30, animate = animationOptions(interval = 600, loop = TRUE))

text_home = list(h1("Presentation"),
                 p("This page is a web application that allows users to interactively explore data related to Severe Acute Respiratory Infection (SARI) in Brazil. This web-based interface provides a practical way for users to visualize data without needing deep technical knowledge of Python or the underlying code."),
                 p("It is supported by a Python package that provides tools for analyzing and processing data related to Severe Acute Respiratory Infection (SARI) and other respiratory viruses. It includes functions for data preprocessing, feature engineering, and training Gradient Boosting Models (GBMs) for binary or multiclass classification, see more ", a(href="https://github.com/joao-1988/PySRAG", "PySRAG"), "."),
                 h2("Features"),
                 tags$ul(
                   tags$li("Data Visualization: Interactive graphs display processed data, giving insights into the distribution of respiratory viruses."),
                   tags$li("Data Filtering: Users can apply filters based on city and patient age to narrow down the data and focus on specific demographics or regions.")
                 ),
                 h2("How to Use"),
                 tags$ol(
                   tags$li("Navigate to the Dashboard: Start on the dashboard, which provides an overview of the visualizations."),
                   tags$li("Apply Filters: Use the filtering options to select specific cities or age ranges to view customized data visualizations."),
                   tags$li("Explore Visualizations: Interact with the visual data representations to gain deeper insights into the trends and patterns.")),
                 h2("Reference"),
                 tags$ul(
                   tags$li("Silva, J.F.A., Soares, G.P., Bastos, L.S., Izbicki, R. (2024). Monitoring Viral Infections in Severe Acute Respiratory Syndrome Patients in Brazil. In: Einbeck, J., Maeng, H., Ogundimu, E., Perrakis, K. (eds) Developments in Statistical Modelling. IWSM 2024. Contributions to Statistics. Springer, Cham. ", a(href="https://doi.org/10.1007/978-3-031-65723-8_12","DOI"),"." ) )
                 
)

body <- dashboardBody(
  ## Itens ----
  tabItems(    
    ## Pagina inicial ----
    tabItem(tabName = "inicial",
            fluidRow(
              column(width = 1),
              column(width = 8, text_home),
              column(width = 3)
            )
    ), 
    ## Prevalencia Pontual----
    tabItem(tabName = "prevalencia_pontual",
            box(width = NULL, height = NULL,
                plotlyOutput("barplot_pontual")
            ),
            column(width = 3, box(width = NULL, height = NULL, radioButtons_view)),
            column(width = 3, box(width = NULL, height = NULL, selectInput_estado_pontual)),
            column(width = 3, box(width = NULL, height = NULL, uiOutput("selectInput_municipios_pontual"))),
            column(width = 3, box(width = NULL, height = NULL, sliderInput_idade_pontual)),
    ) 
  )
)

ui <- dashboardPage( 
  header,
  sidebar,
  body
)

server <- function(input, output) {
  
  output$selectInput_municipios_pontual <- renderUI({
    itens <- df_municipios %>% filter(ESTADO == input$select_estado_pontual)  %>% .$MUNICIPIO
    selectInput("select_municipio_pontual", h3("Municipality"), selected = 'São Paulo', choices = itens)  
  })
  
  output$barplot_pontual <- renderPlotly({
    tryCatch({
      
      if(input$radio_view == 'last_12'){
        df_semanas_aux = df_semanas %>% arrange(-ANO_SEM_SIN_PRI) %>% head(53)
      }else{
        df_semanas_aux = df_semanas 
      }
      
      df_prevalence_point <- df_municipios %>%
        filter(ESTADO == input$select_estado_pontual, MUNICIPIO == input$select_municipio_pontual) %>%
        mutate(IDADE_ANO = input$input_idade_pontual) %>%
        tidyr::crossing(df_semanas_aux)
      
      df_X <- df_prevalence_point %>%
        select(all_of(feature_name$feature_name))
      
      matrix_X <- as.matrix(df_X)
      df_pred_model <- as.data.frame(booster$predict(matrix_X))
      names(df_pred_model) <- classes$virus
      df <- cbind(df_prevalence_point, df_pred_model)
      
      df_sum <- df %>%  
        select(DT_SIN_PRI_SEM, ADENO, BOCA, FLUA, FLUB, METAP, OUTROS, 
               PARA1, PARA2, PARA3, PARA4, RINO, SARS2, VSR) %>% 
        pivot_longer(cols = virus 
                     , names_to = "Vírus", values_to = "Prevalência") %>% 
        rename(Data = DT_SIN_PRI_SEM ) 
      
      # Map the new names
      df_sum$Vírus <- names_map[df_sum$Vírus]
      df_sum$Vírus <- factor(df_sum$Vírus, levels = names_map)
      
      # Adjustments to the data
      df_sum$Data <- df_sum$Data + 6 #Putting the date to the end of week
      df_sum$Prevalência <- df_sum$Prevalência*100 #Changing proportion to percent

      latest_date_str <- format(max(df_sum$Data), "%Y/%m/%d")
      
      title <- paste0("Virus distribution among hospitalized patients who tested positive up to "
                      ,latest_date_str
                      ,"\n"
                      ,"(Updated with data released on "
                      ,latest_update_str
                      ,")" )

      df_sum %>% plot_ly(x = ~Data, y = ~Prevalência, color = ~Vírus, type = 'bar',
                         colors = palette ) %>%
        layout(title = title,
               xaxis = list(title = "Epidemiological Week",
                            tickformat = "%Y/%m/%d" 
               ),
               yaxis = list(title = "% of estimated cases"),
               barmode = 'stack',
               legend=list(title=list(text='     Virus'))
        )
    }, error = function(e){
      # Não faz nada em caso de erro
      return(NULL)
    })
  }) 
}

shinyApp(ui, server)
