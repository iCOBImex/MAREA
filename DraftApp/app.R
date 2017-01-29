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
library(MPAtools)
library(tidyverse)

# Define UI for application that draws a histogram
# ui = dashboardPage(
#   dashboardHeader(),
#   dashboardSidebar(),
#   dashboardBody(

ui <- dashboardPage(
  dashboardHeader(title = "TURFeffect App"),
  dashboardSidebar(
    h1("Recursos"),
    p(
      "Link al ",
      a("Manual", href = "www.turfeffect.org", target = "_blank")
    ),
    p(
      "Link a la ",
      a("Guía de usuario", href = "www.turfeffect.org", target = "_blank")
    ),
    p(
      "Página de ",
      a("TURFeffect", href = "www.turfeffect.org", target = "_blank")
    ),
    p(
      "Enviar comentarios a",
      a(
        "Juan Carlos Villaseñor-Derbez",
        href = "juancarlos@turfeffect.org",
        target = "_blank"
      )
    ),
    p(),
    downloadButton('reporte', 'Descargar Reporte'),
    p(),
    p(),
    p(),
    img(src = "cobi.jpg", width = "60px"),
    img(src = "turf.jpg", width = "60px")
  ),
  dashboardBody(
    useShinyjs(),
    navbarPage(
      "A tool to evaluate the effectiveness of no-take Marine Reserves",
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
              label = "Base biofis",
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
            h2("Data preview:"),
            tabsetPanel(
              tabPanel("Biophysical",
                       tableOutput("preview1")),
              tabPanel("Socioeconomic",
                       tableOutput("preview2")),
              tabPanel("Governance",
                       tableOutput("preview3"))
            )
          )
        )
      ),
      
      #### Fourth tab starts here ################################################################################
      tabPanel(
        img(src = "select.jpg", width = "150px"),
        fluidRow(
          column(3, wellPanel(h1("Comunidad"),
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
          uiOutput("title"),
          
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
        
        # Insert a legend
        fluidRow(column(
          12, offset = 4, img(src = "legend2.gif", width = "500px")
        )),
        
        # Insert a general score
        fluidRow(column(12, offset = 4, valueBoxOutput("totres"))),
        
        # Insert score by categories
        fluidRow(
          column(4,
                 wellPanel(
                   h1("Biofisicos", align = "center"),
                   valueBoxOutput("biores", width = NULL),
                   actionButton("toggle_bio",
                                "Mas/Menos",
                                icon = icon("th-list")),
                   hidden(
                     uiOutput("shannon"),
                     uiOutput("richness"),
                     uiOutput("density"),
                     uiOutput("biomass"),
                     uiOutput("TL"),
                     uiOutput("orgtl50"),
                     uiOutput("natural")
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

server <- function(input, output) {
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
  
  FormatoA = read.csv("www/bio.csv", sep = ",")
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
    head(bioInput())
  })
  
  output$preview2 <- renderTable({
    head(socioInput())
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
    req(input$biophys)
    req(input$comunidad)
    
    if (any(input$obj %in% c(2, 3, 6)) ||
        any(
          input$indB %in% c(
            "Organismos > LT_50",
            "Densidad de especies objetivo",
            "Biomasa de especies objetivo"
          )
        ) ||
        any(
          input$indS %in% c(
            "Arribos de especies objetivo",
            "Ingresos por arribos de especies objetivo"
          )
        )) {
      sp_list <- bioInput() %>%
        filter(Comunidad == input$comunidad,
               RC == RC()) %>%
        group_by(GeneroEspecie) %>%
        summarize(N = n()) %>%
        filter(!is.na(GeneroEspecie))
      
      wellPanel(
        h1("Especies objetivo"),
        checkboxGroupInput(
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
    
    paste(
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
  
  # Define a reactive value for a tibble that stores the analysis results for biophysical indicators
  results_bio <- reactive({
    req(input$biophys)
    req(input$indB)
    req(input$comunidad)
    req(input$rc)
    
    values = list(indB = input$indB,
                  comunidad = input$comunidad)
    
    bio_results(values, bioInput(), res.fun(), con.fun())
  })
  
  # Define a reactive value for a tibble that stores the analysis results for socioeconomic indicators
  results_soc <- reactive({
    req(input$socioeco)
    req(input$indS)
    req(input$comunidad)

    values = list(indS = input$indS,
                  comunidad = input$comunidad)
    
    soc_results(values, socioInput())
  })
  
  
  ### Output for general results ####################################################################
  
  output$totres <- renderValueBox({
    req(input$obj)
    
    model <- turfeffect(
      MPAtools::shannon(bioInput(),
                        input$comunidad),
      reserve = res.fun(),
      control = con.fun(),
      type = "bio"
    )
    
    valueBox(
      value = "General",
      subtitle = valueBoxString(model, "bio"),
      icon = icon("globe"),
      color = bio_score(model)
    )
    
  })
  
  ### Output for biophys indicators ####################################################################
  
  ######################### General Bio#######################
  output$biores <- renderValueBox({
    if (length(results_bio() > 1)){
    biosummary <- results_bio() %>%
      filter(!is.na(e)) %>%
      mutate(
        Valid = length(e),
        Positive = (e > 0) * 1,
        Score = sum(Positive) / Valid * 100
      ) %>%
      select(Score) %>% max()
    
    valueBox(
      value = "General",
      subtitle = paste0(
        formatC(biosummary, digits = 0, format = "f"),
        "% de indicadores positivos"
      ),
      icon = icon("leaf"),
      color = "green"
    )} else {
      valueBox(
        value = "General",
        subtitle = "0 % de indicadores positivos",
        icon = icon("leaf"),
        color = "green"
      )
    }
  })
  
  ######## Toggle Bio output ##################################
  observeEvent(input$toggle_bio, {
    toggle("shannon")
    toggle("richness")
    toggle("density")
    toggle("biomass")
    toggle("TL")
    toggle("orgtl50")
    toggle("natural")
  })
  
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
    } else {
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
  # output$biomass <- renderUI({
  # model <- results_bio()$model[[7]] #The model for biomass is in the seventhelement of the model column
  #
  #     valueBox(
  #       value = "Biomasa",
  #       subtitle = valueBoxString(model),
  #       icon = icon("leaf"),
  #       color = score(model),
  #       width = NULL
  #     )
  #   }
  # })
  
  # ######################## Trophic Level #######################
  # output$TL <- renderUI({
  #   if ("Nivel trofico" %in% input$indB) {
  #     model <- results_bio()$model[[6]] #The model for trophic level is in the sixth element of the model column
  #
  #     valueBox(
  #       value = "Nivel Trófico",
  #       subtitle = valueBoxString(model),
  #       icon = icon("leaf"),
  #       color = x$color,
  #       width = NULL
  #     )
  #   }
  # })
  
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
  

  ### Output for socioeco indicators ####################################################################
  ### General Soc ####################
  output$socres <- renderValueBox({
    
    print(results_soc())
    socsummary <- results_soc() %>%
      filter(!is.na(e)) %>%
      mutate(
        Valid = length(e),
        Positive = (e > 0) * 1,
        Score = sum(Positive) / Valid * 100
      ) %>%
      select(Score) %>% max()
    
    valueBox(
      value = "General",
      subtitle = paste0(
        formatC(socsummary, digits = 0, format = "f"),
        "% de indicadores positivos"
      ),
      icon = icon("leaf"),
      color = "green"
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
        icon = icon("leaf"),
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
        icon = icon("leaf"),
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
      color = "red"
    )
  })
  
  ######## Toggle Gov output ##################################
  
  observeEvent(input$toggle_gov, {
  })
  
  
  ### Output to download ####################################################################
  output$reporte <- downloadHandler(
    # Define a filename based on the input
    filename = c("ReporteTURFeffect.html"),
    
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <-
        file.path(tempdir(), "MyTemplate.Rmd")
      file.copy("MyTemplate.Rmd",
                tempReport,
                overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(title = c("Report trial"),
                     results = results_bio())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      
      rmarkdown::render(
        input = "MyTemplate.Rmd",
        params = params,
        output_file = file,
        envir = new.env(parent = globalenv())
      )
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
