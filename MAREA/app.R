
library(rdrop2)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(MPAtools)
library(xtable)
library(magrittr)
library(ggExtra)
library(tidyverse)

my_max <- 8

# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = img(src = "MAREAlogo.gif", height = "52px")),
  dashboardSidebar(
    h1("Recursos"),
    p("Link a la ", a("Guía de usuario", href = "https://github.com/turfeffect/AppDraft/blob/master/MAREA_Guide.pdf", target = "_blank")),
    p("Página de ", a("TURFeffect", href = "http://.turfeffect.org", target = "_blank")),
    p(a("Formato de access", href = "https://github.com/turfeffect/Docs/blob/master/Governance%20DB/Governance_0509.accdb", target = "_blank"), "para ingreso de datos de gobenanza."),
    p("Enviar comentarios a:", a("contact@turfeffect.org", href = "contact@turfeffect.org", target = "_blank")),
    tags$div(id="google_translate_element",
             tags$script(src = "google_translate.js"),
             tags$script(src="//translate.google.com/translate_a/element.js?cb=googleTranslateElementInit")),
    checkboxInput(inputId = "share",
                  label = "Compartir datos",
                  value = T),
    p("Compartir tus datos fomenta la conservacion de nuestros oceanos al permitirnos construir conocimiento. Proporciona un correo donde podamos contactarte:"),
    textInput(inputId = "email",
              label = "Correo electronico:",
              value = NULL),
    p("Disclaimer: MAREA analyses are based entirely on users' assumptions; Neither of the parties involved in the development of this tool endorse the results.")
  ),
  
  dashboardBody(
    useShinyjs(),
    navbarPage(title = "Evaluacion de Reservas Marinas", id = "Tabs", theme = "myCSS.css",
               
               
               #### First tab starts here ################################################################################
               tabPanel(title = "(1) Introduccion", value = "Intro",
                        fluidRow(column(1, offset = 10,
                                        actionButton(inputId = "b1.2", label = "Siguiente", icon = icon("arrow-right"), class = "btn2"))),
                        mainPanel(
                          wellPanel(
                            h2("Introduccion"),
                            h3("Antes de seguir, asegúrate de leer la guía de usuario de la aplicación,
             así como el manual de evaluación de zonas de no pesca en México. Podrás encontrar los recursos en el menú lateral.")
                          ),
                          img(src = "intro.gif")
                        )
               ),
               #### Second tab starts here ################################################################################
               tabPanel(title = "(2) Objetivos e Indicadores",
                        value = "Obj",
                        fluidRow(column(1, offset = 1,
                                        actionButton(inputId = "b2.1", label = "Anterior", icon = icon("arrow-left"), class = "btn2")),
                                 column(1, offset = 8,
                                        actionButton(inputId = "b2.3", label = "Siguiente", icon = icon("arrow-right"), class = "btn2")
                                 )),
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
                              p("Basandonos en los objetivos seleccionados, nuestra propuesta de indicadores es la siguiente"),
                              column(4, wellPanel(uiOutput("indB"))),
                              column(3, wellPanel(uiOutput("indS"))),
                              column(5, wellPanel(uiOutput("indG")))
                            )
                          ))
                        )
               ),
               
               #### Third tab starts here ################################################################################
               tabPanel(title = "(3) Datos", value = "Data",
                        fluidRow(column(1, offset = 1,
                                        actionButton(inputId = "b3.2", label = "Anterior", icon = icon("arrow-left"), class = "btn2")),
                                 column(1, offset = 8,
                                        actionButton(inputId = "b3.4", label = "Siguiente", icon = icon("arrow-right"), class = "btn2")
                                 )),
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
                                           'Datos socioeconomicos'),
                            p(),
                            downloadButton('downloadD',
                                           'Datos gobernanza')
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
               tabPanel(title = "(4) Seleccionar Reserva", value = "Select",
                        fluidRow(column(1, offset = 1,
                                        actionButton(inputId = "b4.3", label = "Anterior", icon = icon("arrow-left"), class = "btn2")),
                                 column(1, offset = 8,
                                        actionButton(inputId = "b4.5", label = "Siguiente", icon = icon("arrow-right"), class = "btn2")
                                 )),
                        fluidRow(
                          column(3, offset = 2,
                                 wellPanel(h1("Comunidad"),
                                           uiOutput("comunidad"))),
                          column(3, wellPanel(h1("Reserva-Control"),
                                              uiOutput("rc")),
                                 wellPanel(numericInput("ano.imp",
                                                        "Año de implementacion",
                                                        value = NA,
                                                        min = 1990,
                                                        max = 2100,
                                                        step = 1),
                                           uiOutput("res.length"),
                                           uiOutput("res.width")
                                           )
                                 ),
                          column(3, uiOutput("objsp"))
                        )
               ),
               
               #### Fifth tab starts here ################################################################################
               tabPanel(title = "(5) Confirmar", value = "Conf",
                        fluidPage(
                          fluidRow(column(1, offset = 1,
                                          actionButton(inputId = "b5.4", label = "Anterior", icon = icon("arrow-left"), class = "btn2")),
                                   column(1, offset = 8,
                                          actionButton(inputId = "b5.6", label = "Siguiente", icon = icon("arrow-right"), class = "btn2")
                                   )),
                          textOutput("title"),
                          
                          fluidRow(
                            column(3, wellPanel("Objetivos",
                                                tableOutput("objss"))),
                            
                            column(3, wellPanel("Indicadores biofisicos",
                                                tableOutput("indBs"))),
                            
                            column(3, wellPanel("Indicadores socioeconomicos",
                                                tableOutput("indSs"))),
                            
                            column(3, wellPanel("Indicadores de gobernanza",
                                                tableOutput("indGs")))
                          )
                        )
               ),
               
               #### Sixth tab starts here################################################################################
               tabPanel(title = "(6) Resultados", value = "Result",
                        fluidRow(column(1, offset = 1,
                                        actionButton(inputId = "b6.5", label = "Anterior", icon = icon("arrow-left"), class = "btn2"))),
                        fluidRow(
                          column(4, offset = 4,
                                 textOutput("final_title")
                          )),
                        
                        # Insert a legend
                        fluidRow(
                          column(5, offset = 3,
                                 img(src = "legend2.gif", width = "125%")
                          ),
                          column(1, offset = 1,
                                 downloadButton('reporte', 'Descargar Reporte', class = "butt"))),
                        
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
                                   
                                   hidden(
                                     uiOutput("natural"),
                                     h2("Peces", id = "Peces"),
                                     uiOutput("shannon"),
                                     uiOutput("richness"),
                                     uiOutput("density"),
                                     uiOutput("TL"),
                                     uiOutput("biomass")
                                   ),
                                   
                                   hidden(
                                     h2("Invertebrados", id = "Invertebrados"),
                                     uiOutput("shannon_i"),
                                     uiOutput("richness_i"),
                                     uiOutput("density_i")
                                   ),
                                   
                                   hidden(
                                     h2("Especies objetivo", id = "bio_obj"),
                                     h3("Organismos maduros (peces)", id = "LT50"),
                                     uiOutput("orgtl50_1"),
                                     uiOutput("orgtl50_2"),
                                     uiOutput("orgtl50_3"),
                                     uiOutput("orgtl50_4"),
                                     uiOutput("orgtl50_5"),
                                     uiOutput("orgtl50_6"),
                                     uiOutput("orgtl50_7"),
                                     uiOutput("orgtl50_8"),
                                     
                                     h3("Biomasa (peces)", id = "Biomasa"),
                                     uiOutput("biomass_objsp_1"),
                                     uiOutput("biomass_objsp_2"),
                                     uiOutput("biomass_objsp_3"),
                                     uiOutput("biomass_objsp_4"),
                                     uiOutput("biomass_objsp_5"),
                                     uiOutput("biomass_objsp_6"),
                                     uiOutput("biomass_objsp_7"),
                                     uiOutput("biomass_objsp_8"),
                                     
                                     h3("Densidad", id = "Densidad"),
                                     uiOutput("density_objsp_1"),
                                     uiOutput("density_objsp_2"),
                                     uiOutput("density_objsp_3"),
                                     uiOutput("density_objsp_4"),
                                     uiOutput("density_objsp_5"),
                                     uiOutput("density_objsp_6"),
                                     uiOutput("density_objsp_7"),
                                     uiOutput("density_objsp_8")
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
                                     uiOutput("landings"),
                                     uiOutput("income"),
                                     uiOutput("livelihoods"),
                                     
                                     h2("Especies objetivo", id = "soc_obj"),
                                     h3("Arribos", id = "Arribos"),
                                     uiOutput("landings_objsp_1"),
                                     uiOutput("landings_objsp_2"),
                                     uiOutput("landings_objsp_3"),
                                     uiOutput("landings_objsp_4"),
                                     uiOutput("landings_objsp_5"),
                                     uiOutput("landings_objsp_6"),
                                     uiOutput("landings_objsp_7"),
                                     uiOutput("landings_objsp_8"),
                                     
                                     h3("Ingresos", id = "Ingresos"),
                                     uiOutput("income_objsp_1"),
                                     uiOutput("income_objsp_2"),
                                     uiOutput("income_objsp_3"),
                                     uiOutput("income_objsp_4"),
                                     uiOutput("income_objsp_5"),
                                     uiOutput("income_objsp_6"),
                                     uiOutput("income_objsp_7"),
                                     uiOutput("income_objsp_8")
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
                              uiOutput("org_pesquera"),
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
  
  #### Manage the buttons to go back and forth ####
  # The way to read this is that b1.2 changes from tab 1 to tab 2. As b6.5 changes from tab 6 to tab 5.
  
  observeEvent(input$b1.2, {updateNavbarPage(session, "Tabs", selected = "Obj")})
  observeEvent(input$b2.1, {updateNavbarPage(session, "Tabs", selected = "Intro")})
  observeEvent(input$b2.3, {updateNavbarPage(session, "Tabs", selected = "Data")})
  observeEvent(input$b3.2, {updateNavbarPage(session, "Tabs", selected = "Obj")})
  observeEvent(input$b3.4, {updateNavbarPage(session, "Tabs", selected = "Select")})
  observeEvent(input$b4.3, {updateNavbarPage(session, "Tabs", selected = "Data")})
  observeEvent(input$b4.5, {updateNavbarPage(session, "Tabs", selected = "Conf")})
  observeEvent(input$b5.4, {updateNavbarPage(session, "Tabs", selected = "Select")})
  observeEvent(input$b5.6, {updateNavbarPage(session, "Tabs", selected = "Result")})
  observeEvent(input$b6.5, {updateNavbarPage(session, "Tabs", selected = "Conf")})
  
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
  
  FormatoA = read.csv("www/bio_fish.csv", sep = ",", stringsAsFactors = F)
  FormatoB = read.csv("www/bio_invert.csv", sep = ",", stringsAsFactors = F)
  FormatoC = read.csv("www/socio.csv", sep = ",", stringsAsFactors = F)
  FormatoD = read.csv("www/gov.csv", sep = ",", stringsAsFactors = F, na.strings = "")
  
  
  output$downloadA <- downloadHandler(
    filename = function() {
      paste0("Biophysical_fish", ".csv")
    },
    content = function(file) {
      write.csv(FormatoA, file, row.names = F)
    }
  )
  
  output$downloadB <- downloadHandler(
    filename = function() {
      paste0("Biophysical_invert", ".csv")
    },
    content = function(file) {
      write.csv(FormatoB, file, row.names = F)
    }
  )
  
  output$downloadC <- downloadHandler(
    filename = function() {
      paste0("Socioeconomic", ".csv")
    },
    content = function(file) {
      write.csv(FormatoC, file, row.names = F)
    }
  )
  
  output$downloadD <- downloadHandler(
    filename = function() {
      paste0("Governance", ".csv")
    },
    content = function(file) {
      write.csv(FormatoD, file, row.names = F)
    }
  )
  
  
  # Definir datos biofisicos ####################################
  bioInput <- reactive({
    inFile <- input$biophys
    
    if (is.null(inFile)) {
      return(NULL)
    } else {
      data <- read.csv(inFile$datapath, header = T, stringsAsFactors = F, strip.white = T) %>%
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
      data <- read.csv(inFile$datapath, header = T, stringsAsFactors = F, strip.white = T) %>%
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
      data <- read.csv(inFile$datapath, header = T, stringsAsFactors = F, strip.white = T)
      return(data)
    }
  })
  
  #### Definir datos de gobernananza ####################################
  
  gobInput <- reactive({
    inFile <- input$govern
    
    if (is.null(inFile)) {
      return(NULL)
    } else {
      data <- read.csv(inFile$datapath, header = T, stringsAsFactors = F, strip.white = T, na.strings = "")
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
    
    if (any(input$indB %in% c("Organismos > LT_50",
                              "Densidad de especies objetivo",
                              "Biomasa de especies objetivo"), 
            input$indS %in% c("Arribos de especies objetivo",
                              "Ingresos por arribos de especies objetivo")
    )){
      obj_sp_list <- sp_list(fish = bioInput(), invert = bioInput_i(), rc = input$rc)
      
      wellPanel(
        h1("Especie objetivo"),
        checkboxGroupInput("objsp",
                           "Selecciona tus especies objetivo",
                           choices = obj_sp_list$sp,
                           selected = obj_sp_list$sp[1])
      )
    }
  })
  
  output$res.length <- renderUI({
    if("Tamano de la reserva" %in% input$indG & any(input$indB %in% c("Organismos > LT_50",
                                                                  "Densidad de especies objetivo",
                                                                  "Biomasa de especies objetivo"))){
      numericInput("res.length",
                   "Longitud de reserva (m)",
                   value = 0)
    }
  })
  
  output$res.width <- renderUI({
    if("Tamano de la reserva" %in% input$indG & any(input$indB %in% c("Organismos > LT_50",
                                                                  "Densidad de especies objetivo",
                                                                  "Biomasa de especies objetivo"))){
      numericInput("res.width",
                   "Ancho de reserva (m)",
                   value = 0)
    }
  })
  
  # Limitar el numero de especies objetivo a 8
  observe({
    req(input$rc)
    if(length(input$objsp) > my_max){updateCheckboxGroupInput(session, "objsp", selected = tail(input$objsp, my_max))}
  })
  
  objective_species <- reactive({
    sp_list(fish = bioInput(), invert = bioInput_i(), rc = input$rc) %>% 
      filter(sp %in% input$objsp)
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
    selected <- options[as.numeric(input$obj) - 1,]
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
      {.$Sitio} %>% 
      unique()
  })
  
  # Define a reactive for a control site
  con.fun <- reactive({
    req(input$rc)
    con <- bioInput() %>% 
      filter(RC == input$rc, Zona == "Control") %>% 
      {.$Sitio} %>% 
      unique()
  })
  
  # Define a reactive value for a tibble that stores the analysis results for biophysical FISH indicators
  results_bio <- reactive({
    req(input$biophys, input$indB, input$comunidad, input$rc)
    
    fish_species <-  filter(objective_species(), class == "fish")
    
    values = list(indB = input$indB,
                  comunidad = input$comunidad,
                  objsp = fish_species,
                  ano.imp = input$ano.imp)
    
    bio_results(values = values, data = bioInput(), res = res.fun(), con = con.fun())
  })
  
  # Define a reactive value for a tibble that stores the analysis results for biophysical INVERT indicators
  results_bio_i <- reactive({
    req(input$biophys_i, input$indB, input$comunidad, input$rc)
    
    invert_species <-  filter(objective_species(), class == "invert")
    
    values = list(indB = input$indB,
                  comunidad = input$comunidad,
                  objsp = invert_species,
                  ano.imp = input$ano.imp)
    
    bio_results_i(values, bioInput_i(), res.fun(), con.fun())
  })
  
  
  # Define a reactive value for a tibble that stores the analysis results for socioeconomic indicators
  results_soc <- reactive({
    req(input$socioeco, input$indS, input$comunidad)
    
    values = list(indS = input$indS,
                  comunidad = input$comunidad,
                  objsp = objective_species(),
                  ano.imp = input$ano.imp)
    
    soc_results(values = values, data = socioInput())
  })
  
  # Define a reactive value for a tibble that stores the analysis results for governance indicators
  results_gov <- reactive({
    req(gobInput())
    
    values <- list(indG = input$indG,
                   comunidad = input$comunidad,
                   objsp = objective_species(),
                   fish_data = bioInput(),
                   res.length = input$res.length,
                   res.width = input$res.width)
    
    gov_results(values, gobInput(), res.fun())
  })
  
  # Define a title for the scorecard dasbhoard
  output$final_title <- renderText({
    req(input$rc)
    
    paste0("Resultados para la reserva de ", input$rc, " en la comunidad de ", input$comunidad)
  })
  
  # Separate results of objective species
  results_obj <- reactive({
    
    results <- results_bio() %>%
      mutate(class = "fish")
    
    if (isTruthy(input$biophys_i)){
      invert <- results_bio_i() %>% 
        mutate(class = "invert")
      results <- rbind(results, invert)
    } 
    
    results <- results[grepl(x = results$Ind, pattern = "Obj"),] %>% 
      filter(!is.na(e))
    
    return(results)
  })
  
  results_obj_lt <- reactive({
    results <- results_obj()[grepl(x = results_obj()$Ind, pattern = "LT"),]
    return(results)
  })
  
  results_obj_d <- reactive({
    results <- results_obj()[grepl(x = results_obj()$Ind, pattern = "Densidad"),]
    return(results)
  })
  
  results_obj_b <- reactive({
    results <- results_obj()[grepl(x = results_obj()$Ind, pattern = "Biomasa"),]
    return(results)
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
      dplyr::filter(!is.na(e)) %>%
      mutate(Valid = length(e),
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
    toggle("biomass")
    toggle("TL")
    toggle("natural")
    
    toggle("Invertebrados")
    toggle("shannon_i")
    toggle("richness_i")
    toggle("density_i")
    
    toggle("bio_obj")
    toggle("LT50")
    toggle("Biomasa")
    toggle("Densidad")
    
    toggle("orgtl50_1")
    toggle("density_objsp_1")
    toggle("biomass_objsp_1")
    
    toggle("orgtl50_2")
    toggle("density_objsp_2")
    toggle("biomass_objsp_2")
    
    toggle("orgtl50_3")
    toggle("density_objsp_3")
    toggle("biomass_objsp_3")
    
    toggle("orgtl50_4")
    toggle("density_objsp_4")
    toggle("biomass_objsp_4")
    
    toggle("orgtl50_5")
    toggle("density_objsp_5")
    toggle("biomass_objsp_5")
    
    toggle("orgtl50_6")
    toggle("density_objsp_6")
    toggle("biomass_objsp_6")
    
    toggle("orgtl50_7")
    toggle("density_objsp_7")
    toggle("biomass_objsp_7")
    
    toggle("orgtl50_8")
    toggle("density_objsp_8")
    toggle("biomass_objsp_8")
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
  
  ## Especies objetivo ########################################
  
  ######################### Organisms above TL 50 1 #######################
  output$orgtl50_1 <- renderUI({
    if ("Organismos > LT_50" %in% input$indB & dim(results_obj_lt())[1] > 0) {
      valueBox(
        value = input$objsp[1],
        subtitle = paste("Organismos > LT50,", results_obj_lt()$string[1]),
        icon = icon("leaf"),
        color = results_obj_lt()$color[1],
        width = NULL
      )
    }
  })
  
  ######################### Organisms above TL 50 2 #######################
  output$orgtl50_2 <- renderUI({
    if ("Organismos > LT_50" %in% input$indB & dim(results_obj_lt())[1] > 1) {
      valueBox(
        value = input$objsp[2],
        subtitle = paste("Organismos > LT50,", results_obj_lt()$string[2]),
        icon = icon("leaf"),
        color = results_obj_lt()$color[2],
        width = NULL
      )
    }
  })
  
  ######################### Organisms above TL 50 3 #######################
  output$orgtl50_3 <- renderUI({
    if ("Organismos > LT_50" %in% input$indB & dim(results_obj_lt())[1] > 2) {
      valueBox(
        value = input$objsp[3],
        subtitle = paste("Organismos > LT50,", results_obj_lt()$string[3]),
        icon = icon("leaf"),
        color = results_obj_lt()$color[3],
        width = NULL
      )
    }
  })
  
  ######################### Organisms above TL 50 4 #######################
  output$orgtl50_4 <- renderUI({
    if ("Organismos > LT_50" %in% input$indB & dim(results_obj_lt())[1] > 3) {
      valueBox(
        value = input$objsp[4],
        subtitle = paste("Organismos > LT50,", results_obj_lt()$string[4]),
        icon = icon("leaf"),
        color = results_obj_lt()$color[4],
        width = NULL
      )
    }
  })
  
  ######################### Organisms above TL 50 5 #######################
  output$orgtl50_5 <- renderUI({
    if ("Organismos > LT_50" %in% input$indB & dim(results_obj_lt())[1] > 4) {
      valueBox(
        value = input$objsp[5],
        subtitle = paste("Organismos > LT50,", results_obj_lt()$string[5]),
        icon = icon("leaf"),
        color = results_obj_lt()$color[5],
        width = NULL
      )
    }
  })
  
  ######################### Organisms above TL 50 6 #######################
  output$orgtl50_6 <- renderUI({
    if ("Organismos > LT_50" %in% input$indB & dim(results_obj_lt())[1] > 5) {
      valueBox(
        value = input$objsp[6],
        subtitle = paste("Organismos > LT50,", results_obj_lt()$string[6]),
        icon = icon("leaf"),
        color = results_obj_lt()$color[6],
        width = NULL
      )
    }
  })
  
  ######################### Organisms above TL 50 7 #######################
  output$orgtl50_7 <- renderUI({
    if ("Organismos > LT_50" %in% input$indB & dim(results_obj_lt())[1] > 6) {
      valueBox(
        value = input$objsp[7],
        subtitle = paste("Organismos > LT50,", results_obj_lt()$string[7]),
        icon = icon("leaf"),
        color = results_obj_lt()$color[7],
        width = NULL
      )
    }
  })
  
  ######################### Organisms above TL 50 8 #######################
  output$orgtl50_8 <- renderUI({
    if ("Organismos > LT_50" %in% input$indB & dim(results_obj_lt())[1] > 7) {
      valueBox(
        value = input$objsp[8],
        subtitle = paste("Organismos > LT50,", results_obj_lt()$string[8]),
        icon = icon("leaf"),
        color = results_obj_lt()$color[8],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Biomass 1 ################
  output$biomass_objsp_1 <- renderUI({
    if ("Biomasa de especies objetivo" %in% input$indB & dim(results_obj_b())[1] > 0) {
      valueBox(
        value = input$objsp[1],
        subtitle = paste("Biomasa,", results_obj_b()$string[1]),
        icon = icon("leaf"),
        color = results_obj_b()$color[1],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Biomass 2 ################
  output$biomass_objsp_2 <- renderUI({
    if ("Biomasa de especies objetivo" %in% input$indB & dim(results_obj_b())[1] > 1) {
      valueBox(
        value = input$objsp[2],
        subtitle = paste("Biomasa,", results_obj_b()$string[2]),
        icon = icon("leaf"),
        color = results_obj_b()$color[2],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Biomass 3 ################
  output$biomass_objsp_3 <- renderUI({
    if ("Biomasa de especies objetivo" %in% input$indB & dim(results_obj_b())[1] > 2) {
      valueBox(
        value = input$objsp[3],
        subtitle = paste("Biomasa,", results_obj_b()$string[3]),
        icon = icon("leaf"),
        color = results_obj_b()$color[3],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Biomass 4 ################
  output$biomass_objsp_4 <- renderUI({
    if ("Biomasa de especies objetivo" %in% input$indB & dim(results_obj_b())[1] > 3) {
      valueBox(
        value = input$objsp[4],
        subtitle = paste("Biomasa,", results_obj_b()$string[4]),
        icon = icon("leaf"),
        color = results_obj_b()$color[4],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Biomass 5 ################
  output$biomass_objsp_5 <- renderUI({
    if ("Biomasa de especies objetivo" %in% input$indB & dim(results_obj_b())[1] > 4) {
      valueBox(
        value = input$objsp[5],
        subtitle = paste("Biomasa,", results_obj_b()$string[5]),
        icon = icon("leaf"),
        color = results_obj_b()$color[5],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Biomass 6 ################
  output$biomass_objsp_6 <- renderUI({
    if ("Biomasa de especies objetivo" %in% input$indB & dim(results_obj_b())[1] > 5) {
      valueBox(
        value = input$objsp[6],
        subtitle = paste("Biomasa,", results_obj_b()$string[6]),
        icon = icon("leaf"),
        color = results_obj_b()$color[6],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Biomass 7 ################
  output$biomass_objsp_7 <- renderUI({
    if ("Biomasa de especies objetivo" %in% input$indB & dim(results_obj_b())[1] > 6) {
      valueBox(
        value = input$objsp[7],
        subtitle = paste("Biomasa,", results_obj_b()$string[7]),
        icon = icon("leaf"),
        color = results_obj_b()$color[7],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Biomass 8 ################
  output$biomass_objsp_8 <- renderUI({
    if ("Biomasa de especies objetivo" %in% input$indB & dim(results_obj_b())[1] > 7) {
      valueBox(
        value = input$objsp[8],
        subtitle = paste("Biomasa,", results_obj_b()$string[8]),
        icon = icon("leaf"),
        color = results_obj_b()$color[8],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Density 1 ##################
  output$density_objsp_1 <- renderUI({
    if ("Densidad de especies objetivo" %in% input$indB & dim(results_obj_d())[1] > 0) {
      valueBox(
        value = input$objsp[1],
        subtitle = paste("Densidad,", results_obj_d()$string[1]),
        icon = icon("leaf"),
        color = results_obj_d()$color[1],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Density 2 ##################
  output$density_objsp_2 <- renderUI({
    if ("Densidad de especies objetivo" %in% input$indB & dim(results_obj_d())[1] > 1) {
      valueBox(
        value = input$objsp[2],
        subtitle = paste("Densidad,", results_obj_d()$string[2]),
        icon = icon("leaf"),
        color = results_obj_d()$color[2],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Density 3 ##################
  output$density_objsp_3 <- renderUI({
    if ("Densidad de especies objetivo" %in% input$indB & dim(results_obj_d())[1] > 2) {
      valueBox(
        value = input$objsp[3],
        subtitle = paste("Densidad,", results_obj_d()$string[3]),
        icon = icon("leaf"),
        color = results_obj_d()$color[3],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Density 4 ##################
  output$density_objsp_4 <- renderUI({
    if ("Densidad de especies objetivo" %in% input$indB & dim(results_obj_d())[1] > 3) {
      valueBox(
        value = input$objsp[4],
        subtitle = paste("Densidad,", results_obj_d()$string[4]),
        icon = icon("leaf"),
        color = results_obj_d()$color[4],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Density 5 ##################
  output$density_objsp_5 <- renderUI({
    if ("Densidad de especies objetivo" %in% input$indB & dim(results_obj_d())[1] > 4) {
      valueBox(
        value = input$objsp[5],
        subtitle = paste("Densidad,", results_obj_d()$string[5]),
        icon = icon("leaf"),
        color = results_obj_d()$color[5],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Density 6 ##################
  output$density_objsp_6 <- renderUI({
    if ("Densidad de especies objetivo" %in% input$indB & dim(results_obj_d())[1] > 5) {
      valueBox(
        value = input$objsp[6],
        subtitle = paste("Densidad,", results_obj_d()$string[6]),
        icon = icon("leaf"),
        color = results_obj_d()$color[6],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Density 7 ##################
  output$density_objsp_7 <- renderUI({
    if ("Densidad de especies objetivo" %in% input$indB & dim(results_obj_d())[1] > 6) {
      valueBox(
        value = input$objsp[7],
        subtitle = paste("Densidad,", results_obj_d()$string[7]),
        icon = icon("leaf"),
        color = results_obj_d()$color[7],
        width = NULL
      )
    }
  })
  
  ######################### Obj sp Density 8 ##################
  output$density_objsp_8 <- renderUI({
    if ("Densidad de especies objetivo" %in% input$indB & dim(results_obj_d())[1] > 7) {
      valueBox(
        value = input$objsp[8],
        subtitle = paste("Densidad,", results_obj_d()$string[8]),
        icon = icon("leaf"),
        color = results_obj_d()$color[8],
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
    toggle("livelihoods")
    
    toggle("soc_obj")
    toggle("Arribos")
    toggle("landings_objsp_1")
    toggle("landings_objsp_2")
    toggle("landings_objsp_3")
    toggle("landings_objsp_4")
    toggle("landings_objsp_5")
    toggle("landings_objsp_6")
    toggle("landings_objsp_7")
    toggle("landings_objsp_8")
    
    toggle("Ingresos")
    toggle("income_objsp_1")
    toggle("income_objsp_2")
    toggle("income_objsp_3")
    toggle("income_objsp_4")
    toggle("income_objsp_5")
    toggle("income_objsp_6")
    toggle("income_objsp_7")
    toggle("income_objsp_8")
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
  
  ######################### Alternative livelihoods #######################
  # output$income <- renderUI({
  #   if ("Trabajos alternativos a pesca" %in% input$indS) {
  #     valueBox(
  #       value = "Ingresos",
  #       subtitle = results_soc()$string[2],
  #       icon = icon("money"),
  #       color = results_soc()$color[2],
  #       width = NULL
  #     )
  #   }
  # })
  
  ######################### Landings Obj 1 #######################
  output$landings_objsp_1 <- renderUI({
    if ("Arribos de especies objetivo" %in% input$indS & !is.na(results_soc()$color[3])) {
      valueBox(
        value = input$objsp[1],
        subtitle = paste("Arribos,", results_soc()$string[3]),
        icon = icon("money"),
        color = results_soc()$color[3],
        width = NULL
      )
    }
  })
  
  ######################### Landings Obj 2 #######################
  output$landings_objsp_2 <- renderUI({
    if ("Arribos de especies objetivo" %in% input$indS & !is.na(results_soc()$color[4])) {
      valueBox(
        value = input$objsp[2],
        subtitle = paste("Arribos,", results_soc()$string[4]),
        icon = icon("money"),
        color = results_soc()$color[4],
        width = NULL
      )
    }
  })
  
  ######################### Landings Obj 3 #######################
  output$landings_objsp_3 <- renderUI({
    if ("Arribos de especies objetivo" %in% input$indS & !is.na(results_soc()$color[5])) {
      valueBox(
        value = input$objsp[3],
        subtitle = paste("Arribos,", results_soc()$string[5]),
        icon = icon("money"),
        color = results_soc()$color[5],
        width = NULL
      )
    }
  })
  
  ######################### Landings Obj 4 #######################
  output$landings_objsp_4 <- renderUI({
    if ("Arribos de especies objetivo" %in% input$indS & !is.na(results_soc()$color[6])) {
      valueBox(
        value = input$objsp[4],
        subtitle = paste("Arribos,", results_soc()$string[6]),
        icon = icon("money"),
        color = results_soc()$color[6],
        width = NULL
      )
    }
  })
  
  ######################### Landings Obj 5 #######################
  output$landings_objsp_5 <- renderUI({
    if ("Arribos de especies objetivo" %in% input$indS & !is.na(results_soc()$color[7])) {
      valueBox(
        value = input$objsp[5],
        subtitle = paste("Arribos,", results_soc()$string[7]),
        icon = icon("money"),
        color = results_soc()$color[7],
        width = NULL
      )
    }
  })
  
  ######################### Landings Obj 6 #######################
  output$landings_objsp_6 <- renderUI({
    if ("Arribos de especies objetivo" %in% input$indS & !is.na(results_soc()$color[8])) {
      valueBox(
        value = input$objsp[6],
        subtitle = paste("Arribos,", results_soc()$string[8]),
        icon = icon("money"),
        color = results_soc()$color[8],
        width = NULL
      )
    }
  })
  
  ######################### Landings Obj 7 #######################
  output$landings_objsp_7 <- renderUI({
    if ("Arribos de especies objetivo" %in% input$indS & !is.na(results_soc()$color[9])) {
      valueBox(
        value = input$objsp[7],
        subtitle = paste("Arribos,", results_soc()$string[9]),
        icon = icon("money"),
        color = results_soc()$color[9],
        width = NULL
      )
    }
  })
  
  ######################### Landings Obj 8 #######################
  output$landings_objsp_8 <- renderUI({
    if ("Arribos de especies objetivo" %in% input$indS & !is.na(results_soc()$color[10])) {
      valueBox(
        value = input$objsp[8],
        subtitle = paste("Arribos,", results_soc()$string[10]),
        icon = icon("money"),
        color = results_soc()$color[10],
        width = NULL
      )
    }
  })
  
  ######################### Income from landings 1 #######################
  output$income_objsp_1 <- renderUI({
    if ("Ingresos por arribos de especies objetivo" %in% input$indS & !is.na(results_soc()$color[11])) {
      valueBox(
        value = input$objsp[1],
        subtitle = paste("Ingresos,", results_soc()$string[11]),
        icon = icon("money"),
        color = results_soc()$color[11],
        width = NULL
      )
    }
  })
  
  ######################### Income from landings 2 #######################
  output$income_objsp_2 <- renderUI({
    if ("Ingresos por arribos de especies objetivo" %in% input$indS & !is.na(results_soc()$color[12])) {
      valueBox(
        value = input$objsp[2],
        subtitle = paste("Ingresos,", results_soc()$string[12]),
        icon = icon("money"),
        color = results_soc()$color[12],
        width = NULL
      )
    }
  })
  
  ######################### Income from landings 3 #######################
  output$income_objsp_3 <- renderUI({
    if ("Ingresos por arribos de especies objetivo" %in% input$indS & !is.na(results_soc()$color[13])) {
      valueBox(
        value = input$objsp[3],
        subtitle = paste("Ingresos,", results_soc()$string[13]),
        icon = icon("money"),
        color = results_soc()$color[13],
        width = NULL
      )
    }
  })
  
  ######################### Income from landings 4 #######################
  output$income_objsp_4 <- renderUI({
    if ("Ingresos por arribos de especies objetivo" %in% input$indS & !is.na(results_soc()$color[14])) {
      valueBox(
        value = input$objsp[4],
        subtitle = paste("Ingresos,", results_soc()$string[14]),
        icon = icon("money"),
        color = results_soc()$color[14],
        width = NULL
      )
    }
  })
  
  ######################### Income from landings 5 #######################
  output$income_objsp_5 <- renderUI({
    if ("Ingresos por arribos de especies objetivo" %in% input$indS & !is.na(results_soc()$color[15])) {
      valueBox(
        value = input$objsp[5],
        subtitle = paste("Ingresos,", results_soc()$string[15]),
        icon = icon("money"),
        color = results_soc()$color[15],
        width = NULL
      )
    }
  })
  
  ######################### Income from landings 6 #######################
  output$income_objsp_6 <- renderUI({
    if ("Ingresos por arribos de especies objetivo" %in% input$indS & !is.na(results_soc()$color[16])) {
      valueBox(
        value = input$objsp[6],
        subtitle = paste("Ingresos,", results_soc()$string[16]),
        icon = icon("money"),
        color = results_soc()$color[16],
        width = NULL
      )
    }
  })
  
  ######################### Income from landings 7 #######################
  output$income_objsp_7 <- renderUI({
    if ("Ingresos por arribos de especies objetivo" %in% input$indS & !is.na(results_soc()$color[17])) {
      valueBox(
        value = input$objsp[7],
        subtitle = paste("Ingresos,", results_soc()$string[17]),
        icon = icon("money"),
        color = results_soc()$color[17],
        width = NULL
      )
    }
  })
  
  ######################### Income from landings 8 #######################
  output$income_objsp_8 <- renderUI({
    if ("Ingresos por arribos de especies objetivo" %in% input$indS & !is.na(results_soc()$color[18])) {
      valueBox(
        value = input$objsp[8],
        subtitle = paste("Ingresos,", results_soc()$string[18]),
        icon = icon("money"),
        color = results_soc()$color[18],
        width = NULL
      )
    }
  })
  
  ### Output for governance indicators ####################################################################
  
  output$gobres <- renderValueBox({
    req(input$govern)
    
    govsummary <- results_gov() %>%
      filter(!is.na(e)) %>%
      mutate(
        Valid = length(e),
        Score = sum(e) / Valid * 100) %>%
      select(Score) %>%
      max()
    
    valueBox(
      value = "General",
      subtitle = paste0(formatC(govsummary, digits = 1, format = "f"), "% de indicadores positivos"),
      icon = icon("users"),
      color = gen_score(govsummary)
    )
  })
  
  ######## Toggle Gov output ##################################
  
  observeEvent(input$toggle_gov, {
    toggle("acceso")
    toggle("pescadores")
    toggle("reconocimiento")
    toggle("tipo")
    toggle("pesca_ilegal")
    toggle("plan_manejo")
    toggle("procuracion")
    toggle("tamano")
    toggle("org_pesquera")
    toggle("reglas_internas")
    toggle("efectividad")
  })
  
  ######################### Access to fishery #######################
  output$acceso <- renderValueBox({
    valueBox(
      value = "Acceso a pesqueria",
      subtitle = results_gov()$string[1],
      icon = icon("users"),
      color = results_gov()$color[1],
      width = NULL
    )
  })
  
  ######################### Number of fishers #######################
  output$pescadores <- renderValueBox({
    valueBox(
      value = "Numero de pescadores",
      subtitle = results_gov()$string[2],
      icon = icon("users"),
      color = results_gov()$color[2],
      width = NULL
    )
  })
  
  ######################### Legal recognition of the reserve #######################
  
  output$reconocimiento <- renderValueBox({
    valueBox(
      value = "Reconocimiento legal",
      subtitle = results_gov()$string[3],
      icon = icon("users"),
      color = results_gov()$color[3],
      width = NULL
    )
  })
  
  ######################### Type of reserve #######################
  output$tipo <- renderValueBox({
    valueBox(
      value = "Tipo de reserva",
      subtitle = results_gov()$string[4],
      icon = icon("users"),
      color = results_gov()$color[4],
      width = NULL
    )
  })
  
  ######################### Degree of illegal fishing #######################
  
  output$pesca_ilegal <- renderValueBox({
    valueBox(
      value = "Grado de pesca ilegal",
      subtitle = "Bien!",
      icon = icon("users"),
      color = "olive",
      width = NULL
    )
  })
  
  ######################### Management plan #######################
  
  output$plan_manejo <- renderValueBox({
    valueBox(
      value = "Plan de manejo",
      subtitle = results_gov()$string[6],
      icon = icon("users"),
      color = results_gov()$color[6],
      width = NULL
    )
  })
  
  ######################### Enforcement #######################
  output$procuracion <- renderValueBox({
    valueBox(
      value = "Procuracion y vigilancia",
      subtitle = results_gov()$string[7],
      icon = icon("users"),
      color = results_gov()$color[7],
      width = NULL
    )
  })
  
  ######################### Size of reserve #######################
  output$tamano <- renderValueBox({
    valueBox(
      value = "Tamano de la reserva",
      subtitle = results_gov()$string[8],
      icon = icon("users"),
      color = results_gov()$color[8],
      width = NULL
    )
  })
  
  ######################### Fishing Organization #######################
  output$org_pesquera <- renderValueBox({
    valueBox(
      value = "Organizacion pesquera",
      subtitle = results_gov()$string[9],
      icon = icon("users"),
      color = results_gov()$color[9],
      width = NULL
    )
  })
  
  ######################### Internal regulations #######################
  output$reglas_internas <- renderValueBox({
    valueBox(
      value = "Reglamentacion interna",
      subtitle = results_gov()$string[10],
      icon = icon("users"),
      color = results_gov()$color[10],
      width = NULL
    )
  })
  
  ######################### Perceived effectiveness #######################
  
  output$efectividad <- renderValueBox({
    valueBox(
      value = "Efectividad percibida",
      subtitle = results_gov()$string[11],
      icon = icon("users"),
      color = results_gov()$color[11],
      width = NULL
    )
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
  
  observeEvent(input$b5.6, {
    if(input$email == "" & input$share){
    cat("got here")
    print(input$email)
    showModal(modalDialog(
      title = "Correo no proporcionado",
      "Asegurate de proveer un correo electronico en la barra lateral.",
      footer = modalButton("Ok")
      
    ))
    }
  })
  
  ### Output to download ####################################################################
  output$reporte <- downloadHandler(
    
    # Define a filename based on the input
    filename = c("ReporteTURFeffect.pdf"),
    
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "pdfTemplate.Rmd")
      file.copy("pdfTemplate.Rmd", tempReport, overwrite = TRUE)
      
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
      
      if (isTruthy(input$govern)){
        gov <- list(data = gobInput(),
                    results = results_gov())} else {
                      gov <- NULL
      }
      
      # Set up parameters to pass to Rmd document
      params <- list(comunidad = input$comunidad,
                     reserva = res.fun(),
                     rc = input$rc,
                     results_bio = bio1,
                     results_bio_i = bio2,
                     results_soc = soc,
                     results_gov = gov)
      
      if (input$share){
        
        MAREA_data <- params
        MAREA_data$objetivos <- input$obj
        MAREA_data$indB <- input$indB
        MAREA_data$indS <- input$indS
        MAREA_data$indG <- input$indG
        MAREA_data$email <- input$email
        
        saveMAREA(data = MAREA_data, comunidad = input$comunidad, reserva = res.fun())
      }
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      
      withProgress(message = "Generando reporte", value = 0.5, {
        rmarkdown::render(
          input = "pdfTemplate.Rmd",
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
