#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(MPAtools)
library(shiny)
library(tidyverse)
library(shinydashboard)

# Define UI for application that draws a histogram
# ui = dashboardPage(
#   dashboardHeader(),
#   dashboardSidebar(),
#   dashboardBody(

ui <- dashboardPage(
  dashboardHeader(title = "TURFeffect App"),
  dashboardSidebar(
    h1("Recursos"),
    p("Link al ", a("Manual", href="www.turfeffect.org", target="_blank")),
    p("Link a la ", a("Guía de usuario", href="www.turfeffect.org", target="_blank")),
    p("Página de ", a("TURFeffect", href="www.turfeffect.org", target="_blank")),
    p("Enviar comentarios a", a("Juan Carlos Villaseñor-Derbez", href="juancarlos@turfeffect.org", target="_blank"))),
  dashboardBody(
    navbarPage("A tool to evaluate the effectiveness of no-take Marine Reserves",
               #theme = shinythemes::shinytheme("cerulean"),
               
               # p(),
               # img(src="cobi.jpg", width="60px"),
               # img(src="turf.jpg", width="60px"),
               # First tab starts here
               tabPanel(img(src="intro.jpg", width="150px"),
                        mainPanel(img(src="info1.jpg", width="920px"),
                                  p("Antes de seguir, asegúrate de leer la guía de usuario de la aplicación, así como el manual de evaluación de zonas de no pesca en México Podrás encontrar los recursos en el menú lateral.")
                        ),
                        position = c("right")),
               #Second tab starts here
               tabPanel(img(src="objeind.jpg", width="150px"),
                        sidebarLayout(
                          sidebarPanel(
                            h1("Objetivos"),
                            checkboxGroupInput("obj",
                                               "Selecciona tus objetivos",
                                               choices = c("Recuperar especies de interés comercial" = 2,
                                                           "Conservar especies en régimen de protección especial" = 3,
                                                           "Mejorar la productividad pesquera en aguas adyacentes" = 4,
                                                           "Evitar que se llegue a la sobreexplotación" = 5,
                                                           "Recuperar especies sobreexplotadas" = 6,
                                                           "Contribuir al mantenimiento de los procesos biológicos" = 7,
                                                           "Preservar la diversidad biologica y los ecosistemas" = 8))
                          ),
                          mainPanel(
                            wellPanel(
                              fluidRow(
                                h1("Indicadores"),
                                
                                p("Basandonos en los objetivos seleccionados, nuestra propuesta de indicadores es la siguiente"),
                                
                                column(4, wellPanel(
                                  uiOutput("indB"))),
                                
                                column(3, wellPanel(
                                  uiOutput("indS")
                                )),
                                
                                column(5, wellPanel(
                                  uiOutput("indG")
                                ))
                              ))
                          ))
               ),
               
               #Third tab starts here
               tabPanel(img(src="datos.jpg", width="150px"),
                        sidebarLayout(
                          sidebarPanel(
                            fileInput(inputId ="biophys",
                                      label = "Base biofis",
                                      accept = ".csv"),
                            
                            fileInput(inputId ="socioeco",
                                      label = "Base socioeco",
                                      accept = ".csv"),
                            
                            fileInput(inputId ="govern",
                                      label = "Base gobernanza",
                                      accept = ".csv")
                          ),
                          mainPanel("Data preview:",
                                    tableOutput("contents")
                          ))
               ),
               
               # Fourth tab starts here
               tabPanel(img(src="select.jpg", width="150px"),
                        fluidRow(
                          column(3, wellPanel(
                            h1("Comunidad"),
                            uiOutput("comunidad")
                          )),
                          
                          column(3, wellPanel(
                            h1("Reserva-Control"),
                            uiOutput("rc")
                          )),
                          
                          column(3,
                                 uiOutput("objsp"))
                        )
                        
               ),
               
               #Fifth tab starts here
               tabPanel(img(src="conf.jpg", width="150px"),
                        fluidPage(
                          textOutput("title"),
                          
                          fluidRow(
                            column(3, wellPanel("Objetivos",
                                                tableOutput("objss")
                            )),
                            
                            column(3, wellPanel("Indicadores biofisicos",
                                                tableOutput("indBs")
                            )),
                            
                            column(3, wellPanel("Indicadores socioeconomicos",
                                                tableOutput("indSs")
                            )),
                            
                            column(3, wellPanel("Indicadores de gobernanza",
                                                tableOutput("indGs")
                            ))
                            # 
                            # column(2, wellPanel("Comunidad",
                            #                     tableOutput("comss")
                            # )),
                            # 
                            # column(2, wellPanel("Reserva-Control",
                            #                     tableOutput("rcpss")
                            # ))
                          )
                        )
               ),
               
               #Sixth tab starts here
               tabPanel(img(src="res.jpg", width="150px"),
                        
                        fluidRow(column(4, offset = 4, wellPanel("Resultado General",
                                                                 imageOutput("totres")
                        ))),
                        fluidRow(
                          column(4, wellPanel("Resultado Biofísicos",
                                              imageOutput("biores")
                          )),
                          
                          column(4, wellPanel("Resultado Socioeconómicos",
                                              imageOutput("socres")
                          )),
                          
                          column(4, wellPanel("Resultado Gobernanza",
                                              imageOutput("gobres")
                          ))),
                        downloadButton('reporte', 'Descargar Reporte')
               )
    )))

######
# Define server logic
server <- function(input, output) {
  
  ##### Definir indicadores reactivos a los objetivos####
  
  # Definir Indicadores Biofisicos
  output$indB <- renderUI({
    
    checkboxGroupInput("indB",
                       "Biofísicos",
                       choices = c("Índice de diversidad de Shannon",
                                   "Riqueza",
                                   "Organismos > LT_50",
                                   "Densidad",
                                   "Densidad de especies objetivo",
                                   "Perturbación natural",
                                   "Nivel trófico",
                                   "Biomasa",
                                   "Biomasa de especies objetivo"),
                       selected = indB_sel(as.numeric(input$obj)))
  })
  
  # Definir idnciadores Socioeconomicos
  output$indS <- renderUI({
    
    checkboxGroupInput("indS",
                       "Socioeconómicos",
                       choices = c("Arribos",
                                   "Arribos de especies objetivo",
                                   "Ingresos por arribos",
                                   "Ingresos por arribos de especies objetivo",
                                   "Trabajos alternativos a pesca"),
                       selected = indS_sel(as.numeric(input$obj)))
  })
  
  # Definir indicadores de gobernanza
  output$indG <- renderUI({
    
    checkboxGroupInput("indG",
                       "Gobernanza",
                       choices = c("Acceso a la pesquería",
                                   "Número de pescadores",
                                   "Reconocimiento legal de la reserva",
                                   "Tipo de reserva",
                                   "Grado de pesca ilegal",
                                   "Plan de manejo",
                                   "Procuración de la reserva",
                                   "Tamaño de la reserva",
                                   "Razonamiento para el diseño de la reserva",
                                   "Pertenencia a oragnizaciones pesqueras",
                                   "Tipo de organización pesquera",
                                   "Representación",
                                   "Reglamentación interna",
                                   "Efectividad percibida"),
                       selected = indG_sel(as.numeric(input$obj)))
    
  })
  
  #### Cargar datos
  
  options(shiny.maxRequestSize = 200*1024^2)
  
  
  # Definir datos biofisicos
  datasetInput <- reactive({
    
    inFile <- input$biophys
    
    if (is.null(inFile)){
      # data.frame(Comunidad = c("El Rosario", "Maria Elena", "Puerto Libertad"),
      #            Reserva = c("La Caracolera", "El Gallinero", "Cerro Bola"),
      #            Control = c("Lazaro", "El Callienro Control", "Cerro Bola control")) %>%
      #   mutate(RC = paste(Reserva, Control, sep = "-"))
      return(NULL)
      
    } else {
      
      data <- read.csv(inFile$datapath, header = T, stringsAsFactors = F) %>%
        filter(!is.na(Comunidad))
      
      return(data)
    }
  })
  
  output$contents <- renderTable({
    head(datasetInput())
  })
  
  # Definir datos pesqueros
  
  # Definir datos de gobernananza
  
  #######
  ### Definir Comunidades y Reservas-Control reactivas a los datos ingresados
  
  output$comunidad <- renderUI({
    
    datos <- datasetInput()
    
    comunidades <- unique(datos$Comunidad)
    
    radioButtons("comunidad",
                 "Selecciona tus comunidades",
                 choices = comunidades)
  })
  
  RC <- reactive({
    datos <- datasetInput()
    
    return(unique(datos$RC[datos$Comunidad == input$comunidad]))
  })
  
  output$rc <- renderUI({
    
    radioButtons("rc",
                 "Selecciona tus pares Reserva-Control",
                 choices = RC())
    
  })
  
  output$objsp <- renderUI({
    
    if(any(input$obj == c(2, 3, 6)) |
       
       any(input$indB == c("Organismos > LT_50",
                           "Densidad de especies objetivo",
                           "Biomasa de especies objetivo")) |
       
       any(input$indS == c("Arribos de especies objetivo",
                           "Ingresos por arribos de especies objetivo"))){
      
      sp_list <- datasetInput() %>%
        filter(Comunidad == input$comunidad,
               RC == RC()) %>%
        group_by(GeneroEspecie) %>%
        summarize(N = n()) %>%
        filter(!is.na(GeneroEspecie))
      
      wellPanel(
        h1("Especies objetivo"),
        checkboxGroupInput("objsp",
                           "Selecciona tus especies objetivo",
                           choices = sp_list$GeneroEspecie))
    }
    
  })
  
  
  ### Definir tablas de confirmacion
  
  output$objss <- renderTable({
    options <- data.frame(Options = c("Recuperar especies de interés comercial",
                                      "Conservar especies en régimen de protección especial",
                                      "Mejorar la productividad pesquera en aguas adyacentes",
                                      "Evitar que se llegue a la sobreexplotación",
                                      "Recuperar especies sobreexplotadas",
                                      "Contribuir al mantenimiento de los procesos biológicos",
                                      "Preservar la diversidad biologica y los ecosistemas"))
    
    selected <- options[as.numeric(input$obj)-1,]
    
    selected <- paste(seq(1, length(selected)), "- ", selected)
    
    return(selected)
  })
  
  output$indBs <- renderTable({
    indB<- paste(seq(1, length(input$indB)), "- ", input$indB)
    
    return(indB)
  })
  
  output$indSs <- renderTable({
    indS<- paste(seq(1, length(input$indS)), "- ", input$indS)
    
    return(indS)
  })
  
  output$indGs <- renderTable({
    indG<- paste(seq(1, length(input$indG)), "- ", input$indG)
    
    return(indG)
  })
  
  output$title <- renderText({
    paste("El análisis se generará para la reserva de ", input$rc, " en la comunidad de ", input$comunidad)
  })
  
  # output$comss <- renderTable({
  #   input$comunidad
  # })
  # 
  # output$rcpss <- renderTable({
  #   input$rc
  # })
  
  ### Analisis comienza aqui -------------------------------------------------------------------------
  
  # peces <- reactive({datasetInput()})
  # comunidad <- com.fun()
  res.fun <- reactive({
    data.res <- datasetInput()
    as.character(unique(data.res$Sitio[data.res$RC == input$rc & !data.res$Zonificacion=="Control"]))
  })
  
  con.fun <- reactive({
    data.res <- datasetInput()
    as.character(unique(data.res$Sitio[data.res$RC == input$rc & data.res$Zonificacion=="Control"]))
  })
  
  # control <- con.fun()
  # 
  # Dp <- summary(turfeffect(density(peces, comunidad), reserva, control))
  # Sp <- summary(turfeffect(richness(peces, comunidad), reserva, control))
  # Bp <- summary(turfeffect(fish_biomass(peces, comunidad), reserva, control))
  # NT <- summary(turfeffect(trophic(peces, comunidad), reserva, control))
  
  output$totres <- renderImage({
    model <- summary(turfeffect(MPAtools::density(datasetInput(),
                                                  input$comunidad),
                                reserve = res.fun(),
                                control = con.fun()))
    
    x <- data.frame(est = coefficients(model)[7],
                    p = coefficients(model)[28])
    
    list(src = paste("www/", score(x), sep = ""))}, deleteFile = F)
  
  ### Output for biophys indicators
  output$biores <- renderImage({
    model <- summary(turfeffect(MPAtools::density(datasetInput(),
                                                  input$comunidad),
                                reserve = res.fun(),
                                control = con.fun()))
    
    x <- data.frame(est = coefficients(model)[7],
                    p = coefficients(model)[28])
    
    list(src = paste("www/", score(x), sep = ""))
    
  }, deleteFile = F)
  
  ### Output for socioeco indicators
  output$socres <- renderImage({
    model <- summary(turfeffect(MPAtools::density(datasetInput(),
                                                  input$comunidad),
                                reserve = res.fun(),
                                control = con.fun()))
    
    x <- data.frame(est = coefficients(model)[7],
                    p = coefficients(model)[28])
    
    list(src = paste("www/", score(x), sep = ""))
    
  }, deleteFile = F)
  
  ### Output for governance indicators
  
  output$gobres <- renderImage({
    model <- summary(turfeffect(MPAtools::density(datasetInput(),
                                                  input$comunidad),
                                reserve = res.fun(),
                                control = con.fun()))
    
    x <- data.frame(est = coefficients(model)[7],
                    p = coefficients(model)[28])
    
    list(src = paste("www/", score(x), sep = ""))
    
  }, deleteFile = F)
  
  
  ### Output to download
  output$reporte <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), system.file("rmarkdown/templates/report/Reporte/MyTemplate.Rmd", package = "MPAtools"))
      file.copy(system.file("rmarkdown/templates/report/Reporte/MyTemplate.Rmd", package = "MPAtools"), tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(title = c("Report trial"),
                     peces = datasetInput(),
                     comunidad = input$comunidad,
                     reserva = res.fun(),
                     control = con.fun())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      
      rmarkdown::render(
        input = system.file("rmarkdown/templates/report/Reporte/MyTemplate.Rmd", package = "MPAtools"),
        params = list(title = c("Report trial"),
                      peces = datasetInput(),
                      comunidad = input$comunidad,
                      reserva = res.fun(),
                      control = con.fun()),
        output_file = file,
        envir = new.env(parent = globalenv())
      )
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

