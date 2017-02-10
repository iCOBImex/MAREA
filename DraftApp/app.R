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
    p("Link al ", a("Manual", href = "http://www.turfeffect.org", target = "_blank")),
    p("Link a la ", a("Guía de usuario", href = "http://www.turfeffect.org", target = "_blank")),
    p("Página de ", a("TURFeffect", href = "http://.turfeffect.org", target = "_blank")),
    p("Enviar comentarios a JC Villaseñor a:"),
    p("juancarlos@turfeffect.org")
  ),
  dashboardBody(
    useShinyjs(),
    navbarPage("Marine Reserve Evaluation App",
      #theme = shinythemes::shinytheme("cerulean"),
      
      #### First tab starts here ################################################################################
      tabPanel(
        img(src = "intro.jpg", width = "150px"),
        mainPanel(
          img(src = "intro.gif", width = "920px"),
          p(
            "Antes de seguir, asegúrate de leer la guía de usuario de la aplicación, así como el manual de evaluación de zonas de no pesca en México Podrás encontrar los recursos en el menú lateral."
          )
        ),
        position = c("right")
      ),
      #### Second tab starts here ################################################################################
      tabPanel(
        img(src = "objeind.jpg", width = "150px"),
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
        img(src = "datos.jpg", width = "150px"),
        sidebarLayout(
          sidebarPanel(
            fileInput(
              inputId = "biophys",
              label = "Base biofis peces",
              accept = ".csv"
            ),
            
            fileInput(
              inputId = "biophys_i",
              label = "Base biofis inverts",
              accept = ".csv"
            ),
            
            fileInput(
              inputId = "socioeco",
              label = "Base socioeco",
              accept = ".csv"
            ),
            
            fileInput(
              inputId = "govern",
              label = "Base gobernanza",
              accept = ".csv"
            ),
            p(),
            h2("Sample formats"),
            downloadButton('downloadA',
                           'Biophysical data'),
            p(),
            downloadButton('downloadB',
                           'Socioeconomic data')
          ),
          mainPanel(
            h2("Vista previa:"),
            tabsetPanel(
              tabPanel("Biophysica Peces",
                       tableOutput("preview1")),
              tabPanel("Biophysical Inverts",
                       tableOutput("preview1_i")),
              tabPanel("Socioeconomic",
                       tableOutput("preview2")),
              tabPanel("Governance",
                       tableOutput("preview3"))
            )
          )
        )
      ),
      
      #### Fourth tab starts here ################################################################################
      ## I included an offset = 2 to center the elements
      tabPanel(
        img(src = "select.jpg", width = "150px"),
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
        img(src = "conf.jpg", width = "150px"),
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
        img(src = "res.jpg", width = "150px"),
        
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
        # This is how you control background colors with CSS
        # tags$section(tags$style(".sidebar{background-color: lightblue;}")),
        
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
                         icon = icon("th-list"))
            # hidden()
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
  FormatoB = read.csv("www/socio.csv", sep = ",")
  
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
      data <-
        read.csv(inFile$datapath,
                 header = T,
                 stringsAsFactors = F) %>%
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
      data <-
        read.csv(inFile$datapath,
                 header = T,
                 stringsAsFactors = F) %>%
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
      data <-
        read.csv(inFile$datapath,
                 header = T,
                 stringsAsFactors = F)
      return(data)
    }
  })
  
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
      dplyr::select(Ano, Comunidad, Sitio, Zona, ProfundidadInicial, Visibilidad, Temperatura, GeneroEspecie, Talla, Abundancia) %>% 
      head() %>% 
      xtable()
  })
  
  output$preview2 <- renderTable({
    req(input$socioeco)
    head(socioInput()) %>% 
      xtable()
  })
  
  #### Definir datos de gobernananza ####################################
  
  
  ### Definir Comunidades y Reservas-Control reactivas a los datos ingresados ####################################
  
  output$comunidad <- renderUI({
    req(input$biophys)
    
    datos <- bioInput()
    
    comunidades <- unique(datos$Comunidad)
    
    radioButtons("comunidad",
                 "Selecciona tus comunidades",
                 choices = comunidades)
  })
  
  RC <- reactive({
    req(input$comunidad)
    
    pairs <- bioInput() %>%
      filter(Comunidad == input$comunidad) %>%
      group_by(RC) %>%
      summarize(n())
    
    return(pairs$RC)
  })
  
  output$rc <- renderUI({
    req(input$biophys)
    
    pairs <- RC()
    
    radioButtons("rc",
                 "Selecciona tus pares Reserva-Control",
                 choices = pairs)
    
  })
  
  output$objsp <- renderUI({
    req(input$comunidad)
    
    if (any(input$obj %in% c(2, 3, 6)) || 
        any(input$indB %in% c("Organismos > LT_50",
                              "Densidad de especies objetivo",
                              "Biomasa de especies objetivo")) ||
        any(input$indS %in% c("Arribos de especies objetivo",
                              "Ingresos por arribos de especies objetivo")
        )) {
      sp_list <- rbind(bioInput(), bioInput_i()) %>%
        filter(Comunidad == input$comunidad, RC == RC()) %>%
        group_by(GeneroEspecie) %>%
        summarize(N = n()) %>%
        filter(!is.na(GeneroEspecie))
      
      wellPanel(
        h1("Especies objetivo"),
        radioButtons(
          "objsp",
          "Selecciona tus especies objetivo",
          choices = sp_list$GeneroEspecie
        )
      )
    }
    
  })
  
  
  ### Definir tablas de confirmacion ####################################
  
  output$objss <- renderTable({
    req(input$obj)
    
    options <- data.frame(
      Options = c(
        "Recuperar especies de interés comercial",
        "Conservar especies en régimen de proteccion especial",
        "Mejorar la productividad pesquera en aguas adyacentes",
        "Evitar que se llegue a la sobreexplotacion",
        "Recuperar especies sobreexplotadas",
        "Contribuir al mantenimiento de los procesos biologicos",
        "Preservar la diversidad biologica y los ecosistemas"
      )
    )
    
    selected <- options[as.numeric(input$obj) - 1, ]
    
    selected <-
      unname(data.frame(paste(seq(
        1, length(selected)
      ), "- ", selected)))
    
  })
  
  output$indBs <- renderTable({
    req(input$indB)
    
    indB <-
      unname(data.frame(paste(seq(
        1, length(input$indB)
      ), "- ", input$indB)))
  })
  
  output$indSs <- renderTable({
    req(input$indS)
    
    indS <-
      unname(data.frame(paste(seq(
        1, length(input$indS)
      ), "- ", input$indS)))
  })
  
  output$indGs <- renderTable({
    req(input$indG)
    
    indG <-
      unname(data.frame(paste(seq(
        1, length(input$indG)
      ), "- ", input$indG)))
  })
  
  output$title <- renderText({
    req(input$rc)
    
    paste0(
      "El análisis se generará para la reserva de ",
      input$rc,
      " en la comunidad de ",
      input$comunidad
    )
  })
  
  ### Analisis comienza aqui ####################################
  
  # Define a reactive value for a reserve site
  res.fun <- reactive({
    data.res <- bioInput()
    as.character(unique(data.res$Sitio[data.res$RC == input$rc &
                                         !data.res$Zona == "Control"]))
  })
  
  # Defina a reactive value for a control site
  con.fun <- reactive({
    data.res <- bioInput()
    as.character(unique(data.res$Sitio[data.res$RC == input$rc &
                                         data.res$Zona == "Control"]))
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
  
  output$final_title <- renderText({
    req(input$rc)
    
    paste0(
      "Resultados para la reserva de ",
      input$rc,
      " en la comunidad de ",
      input$comunidad
    )
  })
  
  ### Output for biophys indicators ##################################################################
  
  ######################### General Bio #######################
  output$biores <- renderValueBox({
    
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
    toggle("Peces")
    toggle("shannon")
    toggle("richness")
    toggle("density")
    toggle("density_objsp")
    toggle("biomass")
    toggle("biomass_objsp")
    toggle("TL")
    toggle("orgtl50")
    toggle("natural")
    toggle("Invertebrados")
    toggle("shannon_i")
    toggle("richness_i")
    toggle("density_i")
  })
  
  ## PECES ################################################
  
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
  
  ######################### Organisms above TL 50 #######################
  # output$orgtl50 <- renderUI({
  #   if ("Organismos > LT_50" %in% input$indB) {
  # model <- results_bio()$model[[3]] #The model for Organisms > TL 50 is in the third element of the model column
  #
  #     valueBox(
  #       value = "Organismos > LT50",
  #       subtitle = valueBoxString(model),
  #       icon = icon("leaf"),
  #       color = score(model),
  #       width = NULL
  #     )
  #   }
  # })
  
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
