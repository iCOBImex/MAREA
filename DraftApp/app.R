#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(MPAtools)
library(tidyverse)
library(xtable)

# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "MAREA"),
  dashboardSidebar(
    h1("Recursos"),
    p("Link a la ", a("Guía de usuario", href = "http://www.turfeffect.org", target = "_blank")),
    p("Página de ", a("TURFeffect", href = "http://.turfeffect.org", target = "_blank")),
    p("Enviar comentarios a JC Villaseñor a:"),
    p("juancarlos@turfeffect.org")
  ),
  dashboardBody(
    useShinyjs(),
    navbarPage("Marine Reserve Evaluation App",
      # theme = shinythemes::shinytheme("cerulean"),
      # This is how you control background colors with CSS
      # tags$section(tags$style(".sidebar{background-color: lightblue;}")),
      
      #### First tab starts here ################################################################################
      tabPanel(
        img(src = "boton1.gif", width = "150px"),
        mainPanel(
          h3("Antes de seguir, asegúrate de leer la guía de usuario de la aplicación,
             así como el manual de evaluación de zonas de no pesca en México.
             Podrás encontrar los recursos en el menú lateral."),
          img(src = "intro.gif", width = "125%")
        )
      ),
      #### Second tab starts here ################################################################################
      tabPanel(
        img(src = "boton2.gif", width = "150px"),
        sidebarLayout(
          sidebarPanel(
            h1("Objetivos"),
            checkboxGroupInput(
              "obj",
              "Selecciona tus objetivos",
              choices = c(
                "Recuperar especies de interés comercial" = 2,
                "Conservar especies en régimen de proteccion especial" = 3,
                "Mejorar la productividad pesquera en aguas adyacentes" = 4,
                "Evitar que se llegue a la sobreexplotacion" = 5,
                "Recuperar especies sobreexplotadas" = 6,
                "Contribuir al mantenimiento de los procesos biologicos" = 7,
                "Preservar la diversidad biologica y los ecosistemas" = 8
              )
            )
          ),
          mainPanel(wellPanel(
            fluidRow(
              h1("Indicadores"),
              
              p(
                "Basandonos en los objetivos seleccionados, nuestra propuesta de indicadores es la siguiente"
              ),
              
              column(4, wellPanel(uiOutput("indB"))),
              
              column(3, wellPanel(uiOutput("indS"))),
              
              column(5, wellPanel(uiOutput("indG")))
            )
          ))
        )
      ),
      
      #### Third tab starts here ################################################################################
      tabPanel(
        img(src = "boton3.gif", width = "150px"),
        sidebarLayout(
          sidebarPanel(
            fileInput(
              inputId = "biophys",
              label = "Base peces",
              accept = ".csv"
            ),
            
            fileInput(
              inputId = "biophys_i",
              label = "Base invertebrados",
              accept = ".csv"
            ),
            
            fileInput(
              inputId = "socioeco",
              label = "Base socioeconomicos",
              accept = ".csv"
            ),
            
            fileInput(
              inputId = "govern",
              label = "Base gobernanza",
              accept = ".csv"
            ),
            p(),
            h2("Ejemplos"),
            downloadButton('downloadA',
                           'Datos peces'),
            p(),
            downloadButton('downloadB',
                           'Datos invertebrados'),
            p(),
            downloadButton('downloadC',
                           'Datos socioeconomicos')
          ),
          mainPanel(
            h2("Vista previa:"),
            tabsetPanel(
              tabPanel("Peces",
                       tableOutput("preview1")),
              tabPanel("Invertebrados",
                       tableOutput("preview1_i")),
              tabPanel("Socioeconomicos",
                       tableOutput("preview2")),
              tabPanel("Gobernanza",
                       tableOutput("preview3"))
            )
          )
        )
      ),
      
      #### Fourth tab starts here ################################################################################
      ## I included an offset = 2 to center the elements
      tabPanel(
        img(src = "boton4.gif", width = "150px"),
        fluidRow(
          column(3, offset = 2,
                 wellPanel(h1("Comunidad"),
                           uiOutput("comunidad"))),
          
          column(3, wellPanel(h1("Reserva-Control"),
                              uiOutput("rc"))),
          
          column(3,
                 uiOutput("objsp"))
        )
        
      ),
      
      #### Fifth tab starts here ################################################################################
      tabPanel(
        img(src = "boton5.gif", width = "150px"),
        fluidPage(
          textOutput("title"),
          
          fluidRow(
            column(3, wellPanel("Objetivos",
                                tableOutput("objss"))),
            
            column(3, wellPanel(
              "Indicadores biofisicos",
              tableOutput("indBs")
            )),
            
            column(3, wellPanel(
              "Indicadores socioeconomicos",
              tableOutput("indSs")
            )),
            
            column(3, wellPanel(
              "Indicadores de gobernanza",
              tableOutput("indGs")
            ))
          )
        )
      ),
      
      #### Sixth tab starts here################################################################################
      tabPanel(
        img(src = "boton6.gif", width = "150px"),
        
        fluidRow(
          column(4, offset = 4,
                 textOutput("final_title")
          )),
        
        # Insert a legend
        fluidRow(
          column(5, offset = 3,
                 img(src = "legend2.gif", width = "600px")
        ),
        column(1, offset = 1,
               downloadButton('reporte', 'Descargar Reporte', class = "butt"))),
        
        #Lets define some css syle for the button
        tags$head(tags$style(".butt{background-color:green;}
                             .butt{color: white;}
                             .butt{font-weight: bold;}
                             .butt{font-size: 200%;}")),
        
        # Insert a global score
        fluidRow(
          column(4, offset = 4,
                 wellPanel(
                   h1("Global", align = "center"),
                   valueBoxOutput("totres", width = NULL)))),
        
        # Insert score by categories
        fluidRow(
          column(4,
                 wellPanel(
                   h1("Biofisicos", align = "center"),
                   valueBoxOutput("biores", width = NULL),
                   actionButton("toggle_bio",
                                "Mas/Menos",
                                icon = icon("th-list")),
                   
                   h2("Peces"),
                   hidden(
                     uiOutput("shannon"),
                     uiOutput("richness"),
                     uiOutput("orgtl50"),
                     uiOutput("density"),
                     uiOutput("natural"),
                     uiOutput("density_objsp"),
                     uiOutput("TL"),
                     uiOutput("biomass"),
                     uiOutput("biomass_objsp")
                     ),
                   
                   h2("Invertebrados"),
                   hidden(
                     uiOutput("shannon_i"),
                     uiOutput("richness_i"),
                     uiOutput("density_i")
                   )
                 )),
          
          column(4,
                 wellPanel(
                   h1("Socioeconomicos", align = "center"),
                   valueBoxOutput("socres", width = NULL),
                   actionButton("toggle_soc",
                                "Mas/Menos",
                                icon = icon("th-list")),
                   hidden(
                     uiOutput("landings", width = NULL),
                     uiOutput("income", width = NULL)
                   )
                 )),
          
          column(4, wellPanel(
            h1("Gobernanza", align = "center"),
            valueBoxOutput("gobres", width = NULL),
            actionButton("toggle_gov",
                         "Mas/Menos",
                         icon = icon("th-list")),
            hidden(
              uiOutput("acceso"),
              uiOutput("pescadores"),
              uiOutput("reconocimiento"),
              uiOutput("tipo"),
              uiOutput("pesca_ilegal"),
              uiOutput("plan_manejo"),
              uiOutput("procuracion"),
              uiOutput("tamano"),
              uiOutput("razonamiento"),
              uiOutput("org_pesquera"),
              uiOutput("representacion"),
              uiOutput("reglas_internas"),
              uiOutput("efectividad")
            )
            )
            )
        )
      )
    )
  )
)

############################################################################
##                         Define server logic                           ##
###########################################################################

server <- function(input, output, session) {
  ##### Definir indicadores reactivos a los objetivos ######################################################
  
  # Definir Indicadores Biofisicos
  output$indB <- renderUI({
    checkboxGroupInput(
      "indB",
      "Biofísicos",
      choices = c(
        "Indice de diversidad de Shannon",
        "Riqueza",
        "Organismos > LT_50",
        "Densidad",
        "Densidad de especies objetivo",
        "Perturbacion natural",
        "Nivel trofico",
        "Biomasa",
        "Biomasa de especies objetivo"
      ),
      selected = indB_sel(as.numeric(input$obj))
    )
  })
  
  # Definir idnciadores Socioeconomicos
  output$indS <- renderUI({
    checkboxGroupInput(
      "indS",
      "Socioeconomicos",
      choices = c(
        "Arribos",
        "Arribos de especies objetivo",
        "Ingresos por arribos",
        "Ingresos por arribos de especies objetivo",
        "Trabajos alternativos a pesca"
      ),
      selected = indS_sel(as.numeric(input$obj))
    )
  })
  
  # Definir indicadores de gobernanza
  output$indG <- renderUI({
    checkboxGroupInput(
      "indG",
      "Gobernanza",
      choices = c(
        "Acceso a la pesqueria",
        "Numero de pescadores",
        "Reconocimiento legal de la reserva",
        "Tipo de reserva",
        "Grado de pesca ilegal",
        "Plan de manejo",
        "Procuracion de la reserva",
        "Tamano de la reserva",
        "Razonamiento para el diseno de la reserva",
        "Pertenencia a oragnizaciones pesqueras",
        "Tipo de organizacion pesquera",
        "Representacion",
        "Reglamentacion interna",
        "Efectividad percibida"
      ),
      selected = indG_sel(as.numeric(input$obj))
    )
  })
  
  #### Cargar datos ######################################################################
  
  #### Formatos de prueba disponibles ##########################
  options(shiny.maxRequestSize = 200 * 1024 ^ 2)
  
  FormatoA = read.csv("www/bio_fish.csv", sep = ",")
  FormatoB = read.csv("www/bio_invert.csv", sep = ",")
  FormatoC = read.csv("www/socio.csv", sep = ",")
  
  output$downloadA <- downloadHandler(
    filename = function() {
      paste("Biophysical", ".csv")
    },
    content = function(file) {
      write.csv(FormatoA, file, row.names = F)
    }
  )
  
  output$downloadB <- downloadHandler(
    filename = function() {
      paste("Biophysical", ".csv")
    },
    content = function(file) {
      write.csv(FormatoA, file, row.names = F)
    }
  )
  
  output$downloadC <- downloadHandler(
    filename = function() {
      paste("Socioeconomic", ".csv")
    },
    content = function(file) {
      write.csv(FormatoB, file, row.names = F)
    }
  )
  
  
  # Definir datos biofisicos ####################################
  bioInput <- reactive({
    inFile <- input$biophys
    
    if (is.null(inFile)) {
      return(NULL)
    } else {
      data <- read.csv(inFile$datapath, header = T, stringsAsFactors = F) %>%
        filter(!is.na(Comunidad))
      return(data)
    }
  })
  
  # Definir datos biofisicos ####################################
  bioInput_i <- reactive({
    inFile <- input$biophys_i
    
    if (is.null(inFile)) {
      return(NULL)
    } else {
      data <- read.csv(inFile$datapath, header = T, stringsAsFactors = F) %>%
        filter(!is.na(Comunidad))
      return(data)
    }
  })
  
  
  # Definir datos pesqueros ######################################
  
  socioInput <- reactive({
    inFile <- input$socioeco
    
    if (is.null(inFile)) {
      return(NULL)
      
    } else {
      data <- read.csv(inFile$datapath, header = T, stringsAsFactors = F)
      return(data)
    }
  })
  
  #### Definir datos de gobernananza ####################################
  
  gobInput <- reactive({
    inFile <- input$govern
    
    if (is.null(inFile)) {
      return(NULL)
      
    } else {
      data <- read.csv(inFile$datapath, header = T, stringsAsFactors = F)
      return(data)
    }
  })
  
  ### Preview uploaded datasets
  
  output$preview1 <- renderTable({
    req(input$biophys)
    bioInput() %>% 
      dplyr::select(Ano, Comunidad, Sitio, Zona, ProfundidadInicial, Visibilidad, Temperatura, GeneroEspecie, Talla, Abundancia) %>% 
      head() %>% 
      xtable()
  })
  
  output$preview1_i <- renderTable({
    req(input$biophys_i)
    bioInput_i() %>% 
      dplyr::select(Ano, Comunidad, Sitio, Zona, ProfundidadInicial, Visibilidad, Temperatura, GeneroEspecie, Abundancia) %>% 
      head() %>% 
      xtable()
  })
  
  output$preview2 <- renderTable({
    req(input$socioeco)
    head(socioInput()) %>% 
      xtable()
  })
  
  output$preview3 <- renderTable({
    req(input$govern)
    head(gobInput()) %>% 
      xtable()
  })
  
  ### Definir Comunidades y Reservas-Control reactivas a los datos ingresados ####################################
  
  ## Definir comunidades disponibles en los datos
  output$comunidad <- renderUI({
    req(input$biophys)
    
    datos <- bioInput()
    
    comunidades <- unique(datos$Comunidad)
    
    radioButtons("comunidad",
                 "Selecciona tus comunidades",
                 choices = comunidades)
  })
  
  # Generar pares RC para elegir por el usuario
  output$rc <- renderUI({
    req(input$comunidad)
    
    RC <- bioInput() %>%
      filter(Comunidad == input$comunidad) %>%
      group_by(RC) %>%
      summarize(n()) %>% 
      select(RC)
    
    radioButtons("rc",
                 "Selecciona tu par de Reserva-Control",
                 choices = RC$RC)
  })
  
  # Generar lista de especies en bioInput que estan en los sitios seleccionados
  output$objsp <- renderUI({
    req(input$rc)
    
    if (any(input$obj %in% c(2, 3, 6)) || 
        any(input$indB %in% c("Organismos > LT_50",
                              "Densidad de especies objetivo",
                              "Biomasa de especies objetivo")) ||
        any(input$indS %in% c("Arribos de especies objetivo",
                              "Ingresos por arribos de especies objetivo")
        )){
      sp_list <- bioInput() %>%
        filter(!is.na(GeneroEspecie), RC %in% input$rc) %>%
        select(GeneroEspecie) %>% 
        unique()
      
      wellPanel(
        h1("Especie objetivo"),
        radioButtons("objsp",
                     "Selecciona tus especies objetivo",
                     choices = sp_list$GeneroEspecie)
      )
    }
  })
  
  
  ### Definir tablas de confirmacion ####################################
  
  ### Definir objetivos seleccionados ####
  output$objss <- renderTable({
    req(input$obj)
    
    # Define a data.frame with all the objectives, because input$obj converts them to numbers
    # to deal with them easier thoughout the App
    options <- data.frame(Options = c("Recuperar especies de interés comercial",
                                      "Conservar especies en régimen de proteccion especial",
                                      "Mejorar la productividad pesquera en aguas adyacentes",
                                      "Evitar que se llegue a la sobreexplotacion",
                                      "Recuperar especies sobreexplotadas",
                                      "Contribuir al mantenimiento de los procesos biologicos",
                                      "Preservar la diversidad biologica y los ecosistemas")
    )
    
    # Extract the selected ones, and add a number before them
    selected <- options[as.numeric(input$obj) - 1, ]
    selected <- unname(data.frame(paste(seq(1, length(selected)), "- ", selected)))
  })
  
  ### Confirm biophysical indicators ####
  output$indBs <- renderTable({
    req(input$indB)
    # Extract selected indicators and add a number before them
    indB <- unname(data.frame(paste(seq(1, length(input$indB)), "- ", input$indB)))
  })
  
  #### Extract socioeconomic indicators ####
  output$indSs <- renderTable({
    req(input$indS)
    # Extract selected indicators and add a number before them
    indS <-
      unname(data.frame(paste(seq(
        1, length(input$indS)
      ), "- ", input$indS)))
  })
  
  output$indGs <- renderTable({
    req(input$indG)
    # Extract selected indicators and add a number before them
    indG <-
      unname(data.frame(paste(seq(
        1, length(input$indG)
      ), "- ", input$indG)))
  })
  
  output$title <- renderText({
    req(input$rc)
    paste0("El análisis se generará para la reserva de ", input$rc, " en la comunidad de ", input$comunidad)
  })
  
  ### Analisis comienza aqui ####################################
  
  # Define a reactive for a reserve site
  res.fun <- reactive({
    req(input$rc)
    res <- bioInput() %>% 
      filter(RC == input$rc, !Zona == "Control") %>% 
      select(Sitio) %>% 
      unique()
    
    res$Sitio
  })
  
  # Defina a reactive for a control site
  con.fun <- reactive({
    req(input$rc)
    con <- bioInput() %>% 
      filter(RC == input$rc, Zona == "Control") %>% 
      select(Sitio) %>% 
      unique()
    
    con$Sitio
  })
  
  # Define a reactive value for a tibble that stores the analysis results for biophysical FISH indicators
  results_bio <- reactive({
    req(input$biophys, input$indB, input$comunidad, input$rc)

    values = list(indB = input$indB,
                  comunidad = input$comunidad,
                  objsp = input$objsp)
    
    bio_results(values, bioInput(), res.fun(), con.fun())
  })
  
  # Define a reactive value for a tibble that stores the analysis results for biophysical INVERT indicators
  results_bio_i <- reactive({
    req(input$biophys_i, input$indB, input$comunidad, input$rc)
    
    values = list(indB = input$indB,
                  comunidad = input$comunidad)
    
    bio_results_i(values, bioInput_i(), res.fun(), con.fun())
  })
  

  # Define a reactive value for a tibble that stores the analysis results for socioeconomic indicators
  results_soc <- reactive({
    req(input$socioeco, input$indS, input$comunidad)

    values = list(indS = input$indS,
                  comunidad = input$comunidad)
    
    soc_results(values, socioInput())
  })
  
  # Define a reactive value for a tibble that stores the analysis results for governance indicators
  
  results_gov <- reactive({
    req(gobInput())
    
    values <- list(indG = input$indG,
                   comunidad = input$comunidad)
    
    gov_results(values, gobInput(), res.fun())
    
  })
  
  # Define a title for the scorecard dasbhoard
  
  output$final_title <- renderText({
    req(input$rc)
    
    paste0("Resultados para la reserva de ", input$rc, " en la comunidad de ", input$comunidad)
  })
  
  ### Output for biophys indicators ##################################################################
  
  ######################### General Bio #######################
  output$biores <- renderValueBox({
    req(input$biophys)
    
    if (isTruthy(input$biophys) & isTruthy(input$biophys_i)){
      all_bio_results <- rbind(results_bio(), results_bio_i())
    } else if (isTruthy(input$biophys)){
      all_bio_results <- results_bio()
    }
  
    biosummary <- all_bio_results %>%
      filter(!is.na(e)) %>%
      mutate(
        Valid = length(e),
        Positive = (e > 0) * 1,
        Score = sum(Positive) / Valid * 100) %>%
      select(Score) %>%
      max()
    
    valueBox(
      value = "General",
      subtitle = paste0(formatC(biosummary, digits = 1, format = "f"), "% de indicadores positivos"),
      icon = icon("leaf"),
      color = gen_score(biosummary)
    )
  })
  
  ######## Toggle Bio output ##################################
  observeEvent(input$toggle_bio, {
    toggle("shannon")
    toggle("richness")
    toggle("density")
    toggle("density_objsp")
    toggle("biomass")
    toggle("biomass_objsp")
    toggle("TL")
    toggle("orgtl50")
    toggle("natural")
    toggle("shannon_i")
    toggle("richness_i")
    toggle("density_i")
  })
  
  ## PECES ################################################
  
  # Thu Feb 09 20:46:40 2017 ------------------------------
  # All this will be modularized. I just dont want to brake it
  # before we send to adivisors. In theory, about 400 lines of
  # of code can be saved by modularizing all this. For now, I
  # hope that the comments above each chunck provide enough
  # information about what is happening.
  
  ######################### Shannon #######################
  output$shannon <- renderUI({
    if ("Indice de diversidad de Shannon" %in% input$indB) {
      valueBox(
        value = "Índice de Shannon",
        subtitle = results_bio()$string[1],
        icon = icon("leaf"),
        color = results_bio()$color[1],
        width = NULL
      )
    }
  })
  
  ######################### Richness #######################
  output$richness <- renderUI({
    if ("Riqueza" %in% input$indB) {
      valueBox(
        value = "Riqueza",
        subtitle = results_bio()$string[2],
        icon = icon("leaf"),
        color = results_bio()$color[2],
        width = NULL
      )
    }
  })
  
  ######################### Organisms above TL 50 #######################
  output$orgtl50 <- renderUI({
    if ("Organismos > LT_50" %in% input$indB) {
      valueBox(
        value = "Organismos > LT_50",
        subtitle = results_bio()$string[3],
        icon = icon("leaf"),
        color = results_bio()$color[3],
        width = NULL
      )
    }
  })
  
  ######################### Density #######################
  output$density <- renderUI({
    if ("Densidad" %in% input$indB) {
      valueBox(
        value = "Densidad",
        subtitle = results_bio()$string[4],
        icon = icon("leaf"),
        color = results_bio()$color[4],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Density ##################
  output$density_objsp <- renderUI({
    if ("Densidad de especies objetivo" %in% input$indB) {
      valueBox(
        value = paste("Densidad de", input$objsp),
        subtitle = results_bio()$string[5],
        icon = icon("leaf"),
        color = results_bio()$color[5],
        width = NULL
      )
    }
  })
  
  ######################## Trophic Level #######################
  output$TL <- renderUI({
    if ("Nivel trofico" %in% input$indB) {
      model <- results_bio()$model[[6]] #The model for trophic level is in the sixth element of the model column
      
      valueBox(
        value = "Nivel Trófico",
        subtitle = results_bio()$string[6],
        icon = icon("leaf"),
        color = results_bio()$color[6],
        width = NULL
      )
    }
  })
  
  ######################### Biomass #######################
  output$biomass <- renderUI({
    if ("Biomasa" %in% input$indB) {
      valueBox(
        value = "Biomasa",
        subtitle = results_bio()$string[7],
        icon = icon("leaf"),
        color = results_bio()$color[7],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Biomass ################
  output$biomass_objsp <- renderUI({
    if ("Biomasa de especies objetivo" %in% input$indB) {
      valueBox(
        value = paste("Biomasa de", input$objsp),
        subtitle = results_bio()$string[8],
        icon = icon("leaf"),
        color = results_bio()$color[8],
        width = NULL
      )
    }
  })
  
  ## INVERTEBRADOS ########################################
  
  ######################### Shannon #######################
  output$shannon_i <- renderUI({
    if ("Indice de diversidad de Shannon" %in% input$indB) {
      valueBox(
        value = "Índice de Shannon",
        subtitle = results_bio_i()$string[1],
        icon = icon("leaf"),
        color = results_bio_i()$color[1],
        width = NULL
      )
    }
  })
  
  ######################### Richness #######################
  output$richness_i <- renderUI({
    if ("Riqueza" %in% input$indB) {
      valueBox(
        value = "Riqueza",
        subtitle = results_bio_i()$string[2],
        icon = icon("leaf"),
        color = results_bio_i()$color[2],
        width = NULL
      )
    }
  })
  
  ######################### Density #######################
  output$density_i <- renderUI({
    if ("Densidad" %in% input$indB) {
      valueBox(
        value = "Densidad",
        subtitle = results_bio_i()$string[3],
        icon = icon("leaf"),
        color = results_bio_i()$color[3],
        width = NULL
      )
    }
  })
  
  
  ### Output for socioeco indicators ####################################################################
  ### General Soc ####################
  output$socres <- renderValueBox({
      socsummary <- results_soc() %>%
      filter(!is.na(e)) %>%
      mutate(
        Valid = length(e),
        Positive = (e > 0) * 1,
        Score = sum(Positive) / Valid * 100) %>%
      select(Score) %>%
        max()
    
    valueBox(
      value = "General",
      subtitle = paste0(formatC(socsummary, digits = 1, format = "f"), "% de indicadores positivos"),
      icon = icon("money"),
      color = gen_score(socsummary)
    )

  })
  
  ######## Toggle Soc output ##################################
  observeEvent(input$toggle_soc, {
    toggle("landings")
    toggle("income")
  })

  ######################### Landings #######################
  output$landings <- renderUI({
    if ("Arribos" %in% input$indS) {
      valueBox(
        value = "Arribos",
        subtitle = results_soc()$string[1],
        icon = icon("money"),
        color = results_soc()$color[1],
        width = NULL
      )
    }
  })

  ######################### Income from landings #######################
  output$income <- renderUI({
    if ("Ingresos por arribos" %in% input$indS) {
      valueBox(
        value = "Ingresos",
        subtitle = results_soc()$string[2],
        icon = icon("money"),
        color = results_soc()$color[2],
        width = NULL
      )
    }
  })

  
  ### Output for governance indicators ####################################################################
  
  output$gobres <- renderValueBox({
    valueBox(
      value = "Governance",
      subtitle = "nothing yet",
      icon = icon("users"),
      color = "red",
      width = NULL
    )
  })
  
  ######## Toggle Gov output ##################################
  
  observeEvent(input$toggle_gov, {
  })
  
  ### Output for general results ####################################################################
  
  output$totres <- renderValueBox({
    req(input$obj)
    
    if (isTruthy(input$biophys) & isTruthy(input$biophys_i) & isTruthy(input$socioeco)){
      results <- rbind(results_bio(), results_bio_i(), results_soc())
    } else if (isTruthy(input$biophys) & isTruthy(input$biophys_i)){
      results <- rbind(results_bio(), results_bio_i())
    } else if (isTruthy(input$biophys) & isTruthy(input$socioeco)) {
      results <- rbind(results_bio(), results_soc())
    } else if(isTruthy(input$biophys)) {
      results <- results_bio()
    }
    
    summary <- results %>%
      filter(!is.na(e)) %>%
      mutate(
        Valid = length(e),
        Positive = (e > 0) * 1,
        Score = sum(Positive) / Valid * 100
      ) %>%
      select(Score) %>%
      max()
    
    valueBox(
      value = "General",
      subtitle = paste0(formatC(summary, digits = 1, format = "f"), "% de indicadores positivos"),
      icon = icon("globe"),
      color = gen_score(summary),
      width = NULL
    )
  })
  
  
  ### Output to download ####################################################################
  output$reporte <- downloadHandler(
    # Define a filename based on the input
    filename = c("ReporteTURFeffect.html"),
    
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "MyTemplate.Rmd")
      file.copy("MyTemplate.Rmd", tempReport, overwrite = TRUE)
      
      ### Check that all results exist, or generate null results instead
      
      if (isTruthy(input$biophys)){
        bio1 <- results_bio()} else {
        bio1 <- NULL
      }
      
      if (isTruthy(input$biophys_i)){
        bio2 <- results_bio_i()} else {
        bio2 <- NULL
      }
      
      if (isTruthy(input$socioeco)){
        soc <- results_soc()} else {
        soc <- NULL
      }
      
      # Set up parameters to pass to Rmd document
      params <- list(title = paste("Reporte para", input$rc, input$comunidad),
                     results_bio = bio1,
                     results_bio_i = bio2,
                     results_soc = soc)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      
      withProgress(message = "Generando reporte", value = 0.5, {
        rmarkdown::render(
          input = "MyTemplate.Rmd",
          params = params,
          output_file = file,
          envir = new.env(parent = globalenv())
          
        )
        incProgress(0.5)
      })
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
