######################################################

###                     Shiny                    #####

######################################################

is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("shiny") == FALSE) {install.packages("shiny")} #si openxlsx no est? instalado hago que me lo instale automaticamente

library(shiny)

ui <- fluidPage(
        sliderInput(inputId = "num",     ## hay diferentes funciones para dar Inputs, sliderInput es una, tambien están submitButton() , fileInput(), selectInput() , passwordInput(), entre varias mas
                    label= "Choose a number",
                    value = 25,
                    min = 1,
                    max = 100),
        
        plotOutput("hist")  ## hay diferentes funciones para dar Outputs, datatableOutput() , htmlOutput() , imageOutput() , textOutput() , entre varias mas
        
)

## Use 3 rules to write the server function

## Save objects to display to output$
## Build objects to display with render*(). Use the render*() function that creates the type of output you wish to make. [renderDatatable() , renderImage() , renderPlot()]. render significa HACER
## Access input values with input$

server <- function(input, output) {
        
        output$hist <- renderPlot({
                title <- "100 random normal values"
                hist(rnorm(input$num), main = title)
        })
}

shinyApp(ui = ui, server = server)
