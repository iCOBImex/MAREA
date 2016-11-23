#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui = navbarPage("A tool to evaluate the effectiveness of Marine Reserves",
                # First tab starts here
                tabPanel("Introduccion",
                         sidebarLayout(
                           sidebarPanel(
                             h1("Recursos"),
                             p("Link al ", a("Manual", href="www.turfeffect.org", target="_blank")),
                             p("Link a la ", a("Guía de usuario", href="www.turfeffect.org", target="_blank")),
                             p("Página de ", a("TURFeffect", href="www.turfeffect.org", target="_blank")),
                             p("Enviar comentarios a", a("Juan Carlos Villaseñor-Derbez", href="juancarlos@turfeffect.org", target="_blank"))),
                           mainPanel(
                             h1("Introducción"),
                             p("Bienvenido a la aplicación TURFeffect. Esta es una herramienta que te permitirá evaluar la efectividad de tu zonas de no pesca. La evaluación se basa en el desempeño de una serie de indicadores ", tags$b("Biofísicos, Socioeconómicos y de Gobernanza"), ". Los indicadores son seleccionados con base en los objetivos indicados, pero la aplicación permite que el usuario (tu!) selecciones indicadores que creas más convenientes o sean de tu mayor interés."),
                             p("Antes de seguir, asegúrate de leer la guía de usuario de la aplicación, así como el manual de evaluación de zonas de no pesca en México Podrás encontrar los recursos en el menú de la derecha.")
                           ),
                           position = c("right"))
                ),
                #Second tab starts here
                tabPanel("Objetivos e Indicadores",
                         sidebarLayout(
                           sidebarPanel(
                             h1("Objetivos"),
                             checkboxGroupInput("obj",
                                                "Selecciona tus objetivos",
                                                choices = c("Recuperar especies de interés comercial" = "A",
                                                            "Conservar especies en régimen de protección especial" = "B",
                                                            "Mejorar la productividad pesquera en aguas adyacentes" = "C",
                                                            "Evitar que se llegue a la sobreexplotación" = "D",
                                                            "Recuperar especies sobreexplotadas" = "E",
                                                            "Contribuir al mantenimiento de los procesos biológicos" = "E",
                                                            "Preservar el hábitat de las especies pesqueras" = "F"),
                                                selected = c("A"))
                           ),
                           mainPanel(
                             wellPanel(
                             fluidRow(
                               h1("Indicadores"),
                               
                               p("Basandonos en los objetivos seleccionados, nuestra propuesta de indicadores es la siguiente"),
                               
                               column(3, wellPanel(
                                 uiOutput("indB"))),
                               
                               column( 3, wellPanel(
                                 checkboxGroupInput("indS",
                                                    "Socioeconómicos",
                                                    choices = c("Arribos",
                                                                "Ingresos por arribos"))
                               )),
                               
                               column(3, wellPanel(
                                 checkboxGroupInput("indG",
                                                    "Gobernanza",
                                                    choices = c("1",
                                                                "2",
                                                                "3",
                                                                "4",
                                                                "5",
                                                                "6"))
                               ))
                             ))
                           ))
                ),
                
                #Third tab starts here
                tabPanel("Inputs",
                         sidebarLayout(
                           sidebarPanel(
                             "Stuff"),
                           mainPanel(
                             "More Stuff"
                           ))
                ),
                
                #Fifth tab starts here
                tabPanel("Confirm",
                         sidebarLayout(
                           sidebarPanel(
                             "Stuff"),
                           mainPanel(
                             "More Stuff"
                           ))
                ),
                
                #Sixth tab starts here
                tabPanel("Results",
                         sidebarLayout(
                           sidebarPanel(
                             "Stuff"),
                           mainPanel(
                             "More Stuff"
                           ))
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$indB <- renderUI({
    
    if (input$obj == "A"){
      
      checkboxGroupInput("indB",
                         "Biofísicos",
                         choices = c("Densidad",
                                     "Riqueza",
                                     "Índice de diversidad de Shannon",
                                     "Biomasa",
                                     "Organismos > LT_50",
                                     "Nivel trófico"),
                         selected = c ("Densidad"))
    } else {
      checkboxGroupInput("indB",
                         "Biofísicos",
                         choices = c("Densidad",
                                     "Riqueza",
                                     "Índice de diversidad de Shannon",
                                     "Biomasa",
                                     "Organismos > LT_50",
                                     "Nivel trófico"),
                         selected = c("Riqueza"))
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

