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
                             h1("Recursos")),
                           mainPanel(
                             h1("Más sobre TURFeffect"),
                             p("www.turfeffect.org")
                           ))
                ),
                #Second tab starts here
                tabPanel("Objetivos e Indicadores",
                         sidebarLayout(
                           sidebarPanel(
                             h1("Objetivos"),
                             checkboxGroupInput("obj",
                                                "Select your objectives",
                                                choices = c("A",
                                                            "B",
                                                            "C",
                                                            "D",
                                                            "E",
                                                            "F"))
                           ),
                           mainPanel(
                             h1("Indicadores"),
                             p("Basandonos en los objetivos seleccionados, nuestra propuesta de indicadores es la siguiente"),
                             checkboxGroupInput("indB",
                                                "Biofísicos",
                                                choices = c("Densidad",
                                                            "Riqueza",
                                                            "Índice de diversidad de Shannon",
                                                            "Biomasa",
                                                            "Organismos > LT_50",
                                                            "Nivel trófico")),
                             checkboxGroupInput("indS",
                                                "Socioeconómicos",
                                                choices = c("Arribos",
                                                            "Ingresos por arribos")),
                             checkboxGroupInput("indG",
                                                "Gobernanza",
                                                choices = c("1",
                                                            "2",
                                                            "3",
                                                            "4",
                                                            "5",
                                                            "6"))
                           ))
                ),
                
                #Third tab starts here
                tabPanel("Indicators",
                         sidebarLayout(
                           sidebarPanel(
                             "Stuff"),
                           mainPanel(
                             "More Stuff"
                           ))
                ),
                
                #Fourth tab starts here
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)

